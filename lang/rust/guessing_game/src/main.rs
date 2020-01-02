use std::io;
use rand::Rng;
use std::cmp::Ordering;

fn main() {
    println!("Guess the number!");

	let secret_number = rand::thread_rng().gen_range(1, 101);

	loop {
		println!("Please input your guess.");

		let mut guess = String::new();

		io::stdin().read_line(&mut guess)
			.expect("Failed to read line");
		
		let guess_number : u32 = match guess.trim().parse() {
			Ok(num) => num,
			Err(_) => {
				println!("Input '{}' is not a number!", guess.trim());
				continue;
			},
		};

		println!("You guessed: {}", guess_number);
	
		match guess_number.cmp(&secret_number) {
			Ordering::Less => println!("Too small!"),
			Ordering::Greater => println!("Too big!"),
			Ordering::Equal => {
				println!("You win!");
				break;
			},
		}
	}
}
