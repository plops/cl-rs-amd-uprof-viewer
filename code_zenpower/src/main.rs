use chrono::{DateTime, Utc};
use crossbeam_channel::bounded;
use glium::glutin;
use glium::glutin::event::{Event, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::window::WindowBuilder;
use glium::{Display, Surface};
use imgui::*;
use imgui::{Context, FontConfig, FontGlyphRanges, FontSource, Ui};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use positioned_io::ReadAt;
use std::collections::VecDeque;
use std::fs::File;
use std::io;
use std::sync::Mutex;
use std::thread::spawn;
use std::time::Instant;
use std::{fs, thread, time};
struct System {
    event_loop: EventLoop<()>,
    display: glium::Display,
    imgui: Context,
    platform: WinitPlatform,
    renderer: Renderer,
    font_size: f32,
}
fn init(title: &str) -> System {
    let title = match title.rfind("/") {
        Some(idx) => title.split_at((idx + 1)).1,
        None => title,
    };
    let event_loop = EventLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let builder = WindowBuilder::new()
        .with_title(title.to_owned())
        .with_inner_size(glutin::dpi::LogicalSize::new(512f64, 512f64));
    let display =
        Display::new(builder, context, &event_loop).expect("failed to initialize display");
    let mut imgui = Context::create();
    imgui.set_ini_filename(None);
    let mut platform = WinitPlatform::init(&mut imgui);
    {
        let gl_window = display.gl_window();
        let window = gl_window.window();
        platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Rounded);
    };
    let renderer = Renderer::init(&mut imgui, &display).expect("failed to initialize renderer");
    return System {
        event_loop,
        display,
        imgui,
        platform,
        renderer,
        font_size: 12.,
    };
}
impl System {
    fn main_loop<F: FnMut(&mut bool, &mut Ui) + 'static>(self, mut run_ui: F) {
        let System {
            event_loop,
            display,
            mut imgui,
            mut platform,
            mut renderer,
            ..
        } = self;
        let mut last_frame = Instant::now();
        event_loop.run(move |event, _, control_flow| match event {
            Event::NewEvents(_) => {
                last_frame = imgui.io_mut().update_delta_time(last_frame);
            }
            Event::MainEventsCleared => {
                let gl_window = display.gl_window();
                platform
                    .prepare_frame(imgui.io_mut(), &(gl_window.window()))
                    .expect("failed to prepare frame");
                gl_window.window().request_redraw();
            }
            Event::RedrawRequested(_) => {
                let mut ui = imgui.frame();
                let mut run = true;
                run_ui(&mut run, &mut ui);
                if !(run) {
                    *control_flow = ControlFlow::Exit;
                };
                let gl_window = display.gl_window();
                let mut target = display.draw();
                target.clear_color_srgb(1.0, 1.0, 1.0, 1.0);
                platform.prepare_render(&ui, gl_window.window());
                let draw_data = ui.render();
                renderer
                    .render(&mut target, draw_data)
                    .expect("rendering failed");
                target.finish().expect("swap buffer failed");
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
            }
            event => {
                let gl_window = display.gl_window();
                platform.handle_event(imgui.io_mut(), gl_window.window(), &event);
            }
        });
    }
}
fn parse(data: &[u8]) -> u64 {
    return data.iter().fold(0, |a, b| {
        return (((10) * (a)) + ((b - b'0') as u64));
    });
}
fn read_int(data: &[u8]) -> io::Result<u64> {
    let mut res = 0;
    for (i, byte) in data.iter().enumerate() {
        match byte {
            b'\n' => {
                res = parse(&(data[0..i]));
                return Ok(res);
            }
            b'0'..=b'9' => (),
            0 => {
                {
                    println!(
                        "{} {}:{} parse zero  i={}  byte={}",
                        Utc::now(),
                        file!(),
                        line!(),
                        i,
                        byte
                    );
                }
                res = parse(&(data[0..i]));
            }
            _ => {
                {
                    println!(
                        "{} {}:{} parse error  i={}  byte={}",
                        Utc::now(),
                        file!(),
                        line!(),
                        i,
                        byte
                    );
                }
                Err(io::Error::new(io::ErrorKind::Other, "int reader fail"))?
            }
        }
    }
    return Ok(res);
}
fn main() {
    let (s, r) = crossbeam_channel::unbounded();
    let history = std::sync::Arc::new(Mutex::new(VecDeque::with_capacity(100)));
    spawn(move || {
        loop {
            let history = history.clone();
            let tup = r.recv().ok().unwrap();
            let mut h = history.lock().unwrap();
            h.push_back(tup);
        }
    });
    let b = std::thread::Builder::new().name("hwmon_reader".into());
    b.spawn(move || {
        let f_SVI2_C_Core =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr1_input").unwrap();
        let f_SVI2_C_SoC =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr2_input").unwrap();
        let f_SVI2_Core =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in1_input").unwrap();
        let f_SVI2_SoC =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in2_input").unwrap();
        let f_SVI2_P_Core =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power1_input").unwrap();
        let f_SVI2_P_SoC =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power2_input").unwrap();
        let f_Tdie =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp1_input").unwrap();
        let f_Tctl =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp2_input").unwrap();
        let f_Tccd1 =
            File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp3_input").unwrap();
        loop {
            let mut buf_SVI2_C_Core = [0; 32];
            let mut buf_SVI2_C_SoC = [0; 32];
            let mut buf_SVI2_Core = [0; 32];
            let mut buf_SVI2_SoC = [0; 32];
            let mut buf_SVI2_P_Core = [0; 32];
            let mut buf_SVI2_P_SoC = [0; 32];
            let mut buf_Tdie = [0; 32];
            let mut buf_Tctl = [0; 32];
            let mut buf_Tccd1 = [0; 32];
            let _bytes_SVI2_C_Core = f_SVI2_C_Core
                .read_at(0, &mut buf_SVI2_C_Core)
                .expect("read_at SVI2_C_Core fail");
            let _bytes_SVI2_C_SoC = f_SVI2_C_SoC
                .read_at(0, &mut buf_SVI2_C_SoC)
                .expect("read_at SVI2_C_SoC fail");
            let _bytes_SVI2_Core = f_SVI2_Core
                .read_at(0, &mut buf_SVI2_Core)
                .expect("read_at SVI2_Core fail");
            let _bytes_SVI2_SoC = f_SVI2_SoC
                .read_at(0, &mut buf_SVI2_SoC)
                .expect("read_at SVI2_SoC fail");
            let _bytes_SVI2_P_Core = f_SVI2_P_Core
                .read_at(0, &mut buf_SVI2_P_Core)
                .expect("read_at SVI2_P_Core fail");
            let _bytes_SVI2_P_SoC = f_SVI2_P_SoC
                .read_at(0, &mut buf_SVI2_P_SoC)
                .expect("read_at SVI2_P_SoC fail");
            let _bytes_Tdie = f_Tdie.read_at(0, &mut buf_Tdie).expect("read_at Tdie fail");
            let _bytes_Tctl = f_Tctl.read_at(0, &mut buf_Tctl).expect("read_at Tctl fail");
            let _bytes_Tccd1 = f_Tccd1
                .read_at(0, &mut buf_Tccd1)
                .expect("read_at Tccd1 fail");
            let v_SVI2_C_Core = read_int(&mut buf_SVI2_C_Core).expect("read_int SVI2_C_Core error");
            let v_SVI2_C_SoC = read_int(&mut buf_SVI2_C_SoC).expect("read_int SVI2_C_SoC error");
            let v_SVI2_Core = read_int(&mut buf_SVI2_Core).expect("read_int SVI2_Core error");
            let v_SVI2_SoC = read_int(&mut buf_SVI2_SoC).expect("read_int SVI2_SoC error");
            let v_SVI2_P_Core = read_int(&mut buf_SVI2_P_Core).expect("read_int SVI2_P_Core error");
            let v_SVI2_P_SoC = read_int(&mut buf_SVI2_P_SoC).expect("read_int SVI2_P_SoC error");
            let v_Tdie = read_int(&mut buf_Tdie).expect("read_int Tdie error");
            let v_Tctl = read_int(&mut buf_Tctl).expect("read_int Tctl error");
            let v_Tccd1 = read_int(&mut buf_Tccd1).expect("read_int Tccd1 error");
            s.send((
                Utc::now(),
                v_SVI2_C_Core,
                v_SVI2_C_SoC,
                v_SVI2_Core,
                v_SVI2_SoC,
                v_SVI2_P_Core,
                v_SVI2_P_SoC,
                v_Tdie,
                v_Tctl,
                v_Tccd1,
            ))
            .unwrap();
        }
    });
    let system = init(file!());
    system.main_loop(move |_, ui| {
        Window::new(im_str!("Hello world"))
            .size([3.00e+2, 1.00e+2], Condition::FirstUseEver)
            .build(ui, || {
                ui.text(im_str!("Hello World"));
                let mouse_pos = ui.io().mouse_pos;
                ui.text(format!("mouse: ({:.1},{:.1})", mouse_pos[0], mouse_pos[1]));
            });
        Window::new(im_str!("recv"))
            .size([2.00e+2, 1.00e+2], Condition::FirstUseEver)
            .build(ui, || {
                ui.text(im_str!("recv"));
            });
    });
}
