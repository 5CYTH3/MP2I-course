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
#include "../testing/test.h"

int main() {
    return 0;
}
    "#
    .to_owned()
}

fn test_lib() -> String {
    r#"#pragma once
#include <time.h>
#include <stdbool.h>
#include <stdio.h>

#define test_void(f) \
({ \
		printf("==================================\n"); \
		printf("[Test] of \"%s\"\n", #f); \
		printf("Type: void\n"); \
		clock_t fst = clock(); \
		f(); \
		clock_t snd = clock(); \
		printf("Function run time: %f s\n", (snd-fst) * 1.0 / CLOCKS_PER_SEC); \
		printf("==================================\n"); \
})

#define test_value(x, s) \
({ \
		printf("==================================\n"); \
		printf("[Test] of \"%s\"\n", #x); \
		printf("Type: typed\n"); \
		clock_t fst = clock(); \
		if (x == s) { \
				printf("Test: PASSED.\n"); \
		} else { \
				printf("Test: FAILED.\n"); \
		} \
		clock_t snd = clock(); \
		printf("Function run time: %f s\n", (snd-fst) * 1.0 / CLOCKS_PER_SEC); \
		printf("=================================="); \
})

#define test(f, s) \
({ \
		printf("==================================\n"); \
		printf("[Test] of \"%s\"\n", #f); \
		printf("Type: typed\n"); \
		clock_t fst = clock(); \
		if (f() == s) { \
				printf("Test: PASSED.\n"); \
		} else { \
				printf("Test: FAILED.\n"); \
		} \
		clock_t snd = clock(); \
		printf("Function run time: %f s\n", (snd-fst) * 1.0 / CLOCKS_PER_SEC); \
		printf("=================================="); \
})        "#
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

    match fs::write(format!("{}/testing/test.h", name), test_lib()) {
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
