use chrono::{DateTime, Utc};
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
use std::fs::File;
use std::io;
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
        {
            println!(
                "{} {}:{} parse  a={}  b={}  (b-b'0')={}",
                Utc::now(),
                file!(),
                line!(),
                a,
                b,
                (b - b'0')
            );
        }
        return (((10) * (a)) + ((b - b'0') as u64));
    });
}
fn read_int(data: &[u8]) -> io::Result<u64> {
    let mut res = 0;
    for (i, byte) in data.iter().enumerate() {
        {
            println!(
                "{} {}:{} parse  i={}  byte={}",
                Utc::now(),
                file!(),
                line!(),
                i,
                byte
            );
        }
        match byte {
            b'\n' => {
                {
                    println!(
                        "{} {}:{} parse newline  i={}  byte={}",
                        Utc::now(),
                        file!(),
                        line!(),
                        i,
                        byte
                    );
                }
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
    let f0 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr1_input").unwrap();
    let f1 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr2_input").unwrap();
    let f2 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in1_input").unwrap();
    let f3 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in2_input").unwrap();
    let f4 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power1_input").unwrap();
    let f5 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power2_input").unwrap();
    let f6 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp1_input").unwrap();
    let f7 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp2_input").unwrap();
    let f8 = File::open("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp3_input").unwrap();
    loop {
        let mut buf0: [u8; (512)] = [0; 512];
        let mut buf1: [u8; (512)] = [0; 512];
        let mut buf2: [u8; (512)] = [0; 512];
        let mut buf3: [u8; (512)] = [0; 512];
        let mut buf4: [u8; (512)] = [0; 512];
        let mut buf5: [u8; (512)] = [0; 512];
        let mut buf6: [u8; (512)] = [0; 512];
        let mut buf7: [u8; (512)] = [0; 512];
        let mut buf8: [u8; (512)] = [0; 512];
        let _bytes0 = f0.read_at(0, &mut buf0).expect("read_at fail");
        let _bytes1 = f1.read_at(0, &mut buf1).expect("read_at fail");
        let _bytes2 = f2.read_at(0, &mut buf2).expect("read_at fail");
        let _bytes3 = f3.read_at(0, &mut buf3).expect("read_at fail");
        let _bytes4 = f4.read_at(0, &mut buf4).expect("read_at fail");
        let _bytes5 = f5.read_at(0, &mut buf5).expect("read_at fail");
        let _bytes6 = f6.read_at(0, &mut buf6).expect("read_at fail");
        let _bytes7 = f7.read_at(0, &mut buf7).expect("read_at fail");
        let _bytes8 = f8.read_at(0, &mut buf8).expect("read_at fail");
        {
            println!("{} {}:{} read  _bytes0={}  _bytes1={}  _bytes2={}  _bytes3={}  _bytes4={}  _bytes5={}  _bytes6={}  _bytes7={}  _bytes8={}", Utc::now(), file!(), line!(), _bytes0, _bytes1, _bytes2, _bytes3, _bytes4, _bytes5, _bytes6, _bytes7, _bytes8);
        }
        let v0 = read_int(&mut buf0).expect("read_int error");
        let v1 = read_int(&mut buf1).expect("read_int error");
        let v2 = read_int(&mut buf2).expect("read_int error");
        let v3 = read_int(&mut buf3).expect("read_int error");
        let v4 = read_int(&mut buf4).expect("read_int error");
        let v5 = read_int(&mut buf5).expect("read_int error");
        let v6 = read_int(&mut buf6).expect("read_int error");
        let v7 = read_int(&mut buf7).expect("read_int error");
        let v8 = read_int(&mut buf8).expect("read_int error");
        {
            println!(
                "{} {}:{}   v0={}  v1={}  v2={}  v3={}  v4={}  v5={}  v6={}  v7={}  v8={}",
                Utc::now(),
                file!(),
                line!(),
                v0,
                v1,
                v2,
                v3,
                v4,
                v5,
                v6,
                v7,
                v8
            );
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr1_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr1_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr2_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/curr2_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in1_input")
                .expect("read error");
        {
            println!(
                "{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in1_input  contents={}",
                Utc::now(),
                file!(),
                line!(),
                contents
            );
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in2_input")
                .expect("read error");
        {
            println!(
                "{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/in2_input  contents={}",
                Utc::now(),
                file!(),
                line!(),
                contents
            );
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power1_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power1_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power2_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/power2_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp1_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp1_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp2_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp2_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
    {
        let contents =
            fs::read_to_string("/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp3_input")
                .expect("read error");
        {
            println!("{} {}:{} /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon0/temp3_input  contents={}", Utc::now(), file!(), line!(), contents);
        };
    }
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
