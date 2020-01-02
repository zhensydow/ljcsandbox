use ferris_says::say;
use std::io;

fn main() -> io::Result<()> {
	let stdout = io::stdout();

	let mut writer = io::BufWriter::new(stdout.lock());
    say(b"Hello, world!", 24, &mut writer).unwrap();

	Ok(())
}
