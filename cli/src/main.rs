use std::{fs::File, io::Read};

use clap::error::Error;

use compiler::compile;
use pretty_env_logger::env_logger;

/// Simple program to greet a person
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The input file
    input_file: Option<String>,

    /// Immediate input. Should not be used with `input`
    #[arg(short, long)]
    input_text: Option<String>,
}

fn open(s: String) -> Result<String, Error> {
    let mut file = match File::open(s) {
        Ok(file) => file,
        Err(error) => panic!("Couldn't open the file: {error}"),
    };

    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => Ok(contents),
        Err(error) => panic!("Couldn't read the file: {error}"),
    }
}

fn main() {
    env_logger::init();
    let args = <Args as clap::Parser>::parse();

    let input_text = if let Some(x) = args.input_text {
        x
    } else if let Some(x) = args.input_file {
        open(x).unwrap()
    } else {
        unreachable!()
    };
    compile_and_render(&input_text);
}

fn compile_and_render(input_text: &str) {
    pretty_env_logger::init();
    let dot = compile(input_text);
    render(dot);
}

use cli_clipboard::{ClipboardContext, ClipboardProvider};
fn render(dot: String) {
    let mut ctx = ClipboardContext::new().unwrap();
    println!("{dot}");
    ctx.set_contents(dot).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_func() {
        let program = r"
            main
            var a; {
                let n <- call InputNum();
                let fib <- call fibbonachi(n);
                call OutputNum(fib);
            }
            function fibbonachi(n: [f32; 3]) {
                let a <- 0;
                let b <- 1;
                let i <- 0;
                while i < n
                do 
                    let i <- i + 1
                    let sum <- a + b;
                    let a <- b;
                    let b <- sum;
                od
                return b;
            }

        ";
        compile_and_render(program);
    }

    #[test]
    fn test_if() {
        let program = r"
            main 
            var x;
            {
                if 1<=1 then 
                    if 2=2 then 
                        if 3=3 then 
                            if 4=4 then
                                call OutputNum(5)
                            fi
                        else
                            call OutputNum(3)
                        fi
                    fi
                fi
            }
        ";
        compile_and_render(program);
    }

    #[test]
    fn test_arrays() {
        let program = r"
            main
            var x,y,i,j;
            array[4] a;
            {
                let i<-call InputNum(); 
                let x<-0; 
                let y<-0; 
                let j<- i; 
                let a[x] <- i; 
                while x<10 do 
                    let x <- i + 1; 
                    let y <- a[x] + 1; 
                    let i <-i + 1 
                od; 
                call OutputNum(x); 
                call OutputNum(a[x])
            }.
        ";
        compile_and_render(program);
    }

    #[test]
    fn test_bubble_sort() {
        let program = r"
            main 
            array[4] a; 
            {
                call bubble_sort(a, 4);
            }

            function bubble_sort(arr: [i32; 10], size) {
                let i <- 0;
                while i < size do
                    let j <- 0;
                    while j < size - i - 1 do 
                        if arr[j] > arr[j+1] then
                            let temp <- arr[j]
                            let arr[j] <- arr[j+1]
                            let arr[j+1] <- temp
                            
                            return 0;
                            
                            let a <- a+b;
                
                        fi
                        call OutputNum(arr[j]);
                        let i <- i + 1;
                    let j <- j+1
                    od
                od
            }
        ";
        compile_and_render(program);
    }

    #[test]
    fn test_while() {
        let program = "
        main
        var i, x, y, j;
        array x[5][6];
        {
            let x[1][2] <- 5
            let i <- call InputNum();
            let x <- 0;
            let y <- 0;
            let j <- i;
            while x < 10 do
                let x <- i + 1;
                let x <- x + 1;
                let y <- j + 1;
                let i <- i + 1;
            od;
            call OutputNum(i);
            call OutputNum(x);
            call OutputNum(j);
            call OutputNum(y)
        }.
        ";
        compile_and_render(program);
    }

    
    #[test]
    fn test_secret() {
        let program = "
main
array[5][5] x;
{
    let x[1][2] <- x[2][7];
}.
        ";
        compile_and_render(program);
    }
}
