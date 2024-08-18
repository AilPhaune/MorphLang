pub trait Parser<InputType, ErrorType, OutputType> {
    fn run(&self, input: &InputType) -> Result<(InputType, OutputType), ErrorType>;
}

impl<InputType, ErrorType, OutputType, Func> Parser<InputType, ErrorType, OutputType> for Func
where
    Func: Fn(&InputType) -> Result<(InputType, OutputType), ErrorType>,
{
    fn run(&self, input: &InputType) -> Result<(InputType, OutputType), ErrorType> {
        self(input)
    }
}

pub fn run_parser<InputType, ErrorType, OutputType>(
    parser: impl Parser<InputType, ErrorType, OutputType>,
    input: &InputType,
) -> Result<(InputType, OutputType), ErrorType> {
    parser.run(input)
}
