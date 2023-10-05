use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    #[builder(eac = "arg")]
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}

fn main() {}
