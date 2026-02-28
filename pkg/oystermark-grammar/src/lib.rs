use pest_derive::Parser;

pub use pest::Parser;

#[derive(Parser)]
#[grammar = "grammars/OysterMarkCore.pest"]
pub struct OysterMarkCoreParser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_atx_heading() {
        let input = "# Hello\n";
        let result = OysterMarkCoreParser::parse(Rule::document, input);
        assert!(
            result.is_ok(),
            "Failed to parse ATX heading: {:?}",
            result.err()
        );
    }

    #[test]
    fn parse_paragraph() {
        let input = "Hello world\n";
        let result = OysterMarkCoreParser::parse(Rule::document, input);
        assert!(
            result.is_ok(),
            "Failed to parse paragraph: {:?}",
            result.err()
        );
    }
}
