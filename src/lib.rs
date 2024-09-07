pub mod analysis;
pub mod parsing;
pub mod preprocessor;
pub mod traits;
pub mod type_checker;

#[macro_export]
macro_rules! assert_is_error_print_ok {
    ($result: expr) => {
        assert!(
            $result.is_err(),
            "Expected an error, but got Ok: {:?}",
            $result.unwrap()
        );
    };
}

#[macro_export]
macro_rules! assert_is_error {
    ($result: expr) => {
        assert!(
            $result.is_err(),
            "Expected an error, but got Ok: {:?}",
            $result.unwrap()
        );
    };
}
