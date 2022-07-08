use std::error::Error;
use std::io::stdin;
use scripten::lexer;
use scripten::lexer::context::LexerContext;
use scripten::lexer::tokens::GetTokens;
use scripten::source::StrSource;
use scripten::lexer::token::Token;

fn main() -> Result<(), Box<dyn Error>> {
    fn push_line<'a>(mut acc: String, line: String) -> String {
        acc.push_str(&line);
        acc.push('\n');
        acc
    }

    let source = stdin()
        .lines()
        .take_while(|line| line.is_ok())
        .map(|line| line.unwrap())
        .take_while(|line| line != "exit")
        .fold(String::new(), push_line);

    let mut context = LexerContext::default();

    let tokens: lexer::Result<Vec<Token>> = StrSource::new(&source).tokens(&mut context).collect();
    let tokens = tokens?;

    for token in tokens {
        println!("{:?}", token)
    }

    Ok(())
}