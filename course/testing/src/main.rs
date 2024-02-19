use std::fs;

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Generate { name: String },
}

fn main_file() -> String {
    r#"#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main() {
    return 0;
}
    "#
    .to_owned()
}

fn test_lib() -> String {
    r#"#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

bool test(void *arr, void (*fun_ptr) (...)) {
    
}
        "#
    .to_owned()
}

fn make_file() -> String {
    r#"main.c:

        "#
    .to_owned()
}

fn generate(name: &String) {
    // Create the  `src` subfolder and the root project folder
    match fs::create_dir_all(format!("{}/src", name)) {
        Err(why) => println!(
            "Error when creating project folder. Error : {:?}",
            why.kind()
        ),
        Ok(_) => {}
    }

    match fs::create_dir_all(format!("{}/testing", name)) {
        Err(why) => println!(
            "Error when creating project folder. Error : {:?}",
            why.kind()
        ),
        Ok(_) => {}
    }

    match fs::write(format!("{}/src/main.c", name), main_file()) {
        Err(why) => println!(
            "Error when creating tne main file. Error : {:?}",
            why.kind()
        ),
        Ok(_) => {}
    }

    match fs::write(format!("{}/testing/test.c", name), test_lib()) {
        Err(why) => println!(
            "Error when creating tne main file. Error : {:?}",
            why.kind()
        ),
        Ok(_) => {}
    }
    match fs::write(format!("{}/Makefile", name), make_file()) {
        Err(why) => println!(
            "Error when creating tne main file. Error : {:?}",
            why.kind()
        ),
        Ok(_) => {}
    }
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Commands::Generate { name } => generate(&name),
    }
}
