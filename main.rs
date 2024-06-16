// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: Option<String>,
    #[builder(each = "arg")]
    args: Vec<String>,
    #[builder(eac = "env")]
    env: Vec<String>,
    current_dir: String,
}

fn main() {
    Command::builder()
        .arg("boo".to_owned())
        .env("uuu".to_owned())
        .current_dir("".to_owned())
        .build()
        .unwrap();
}
