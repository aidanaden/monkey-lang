use rust::repl;

fn main() {
    println!(
        "Hello {}! Welcome to the monkey programming language!",
        whoami::username()
    );
    println!("Enter commands below:");
    repl::repl::start();
}
