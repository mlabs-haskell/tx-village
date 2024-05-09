/// Specify what the indexer event handler should do for specific errors. See: `ErrorPolicyProvider`.
/// The idea is that an error type, `E`, implements `ErrorPolicyProvider`.
/// Based on the different variants of `E`, different `ErrorPolicy` can be returned, which influences
/// the behavior of the event handler.
pub enum ErrorPolicy<E> {
    /// Indicate the callback operation should be retried. Also see: `RetryPolicy`.
    Retry,
    /// Indicate that the error should be ignored, go to next event.
    Skip,
    /// Indicate that the event handler should exit with error.
    Exit,
    /// Indicate that the event handler should call given error handling function with the error.
    Call(fn(E) -> ()),
}

/// Trait that can be implemented for custom error types.
/// Different variants in said error types can then be given different `ErrorPolicy` assignments.
pub trait ErrorPolicyProvider
where
    Self: Sized,
{
    fn get_error_policy(&self) -> ErrorPolicy<Self>;
}
