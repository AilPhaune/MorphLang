pub mod parsing;
pub mod traits;

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
