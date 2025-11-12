use plutus_ledger_api::csl::lib as csl;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum FromPallasError {
    #[error(transparent)]
    CardanoSerializationLibError(#[from] csl::DeserializeError),

    #[error(transparent)]
    CardanoSerializationLibJsError(#[from] csl::JsError),

    #[error(transparent)]
    TryFromCSLError(#[from] plutus_ledger_api::csl::csl_to_pla::TryFromCSLError),

    #[error(transparent)]
    TryFromPLAError(#[from] plutus_ledger_api::csl::pla_to_csl::TryFromPLAError),

    #[error("Couldn't parse the stake address because its header byte was invalid: {0}")]
    InvalidStakeAddressHeaderByte(String),

    #[error("Couldn't parse the stake address because its length is invalid: {0}")]
    InvalidStakeAddressLength(String),
}

pub trait FromPallas<T>
where
    Self: Sized,
{
    fn from_pallas(value: T) -> Result<Self, FromPallasError>;
}

pub trait IntoPallas<T> {
    fn into_pallas(self) -> Result<T, FromPallasError>;
}

impl<T, R> IntoPallas<T> for R
where
    T: FromPallas<R>,
{
    fn into_pallas(self) -> Result<T, FromPallasError> {
        T::from_pallas(self)
    }
}
