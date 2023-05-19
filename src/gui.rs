/// The layer to interact with the GUI
use druid::widget::Label;
use druid::{AppLauncher, Widget, WindowDesc};

fn build_ui() -> impl Widget<()> {
    Label::new("Hello world")
}

pub(crate) fn launch() {
    let main_window = WindowDesc::new(build_ui())
        .window_size((600.0, 400.0))
        .title("My first Druid App");
    let initial_data = ();

    AppLauncher::with_window(main_window)
        .launch(initial_data)
        .expect("Failed to launch application");
}
