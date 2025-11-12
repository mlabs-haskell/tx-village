use pallas_network::miniprotocols;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Clone, PartialEq)]
pub struct BlockHash(pub Vec<u8>);

impl std::fmt::Debug for BlockHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode(&self.0))
    }
}

impl Serialize for BlockHash {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&hex::encode(&self.0))
    }
}
impl<'de> Deserialize<'de> for BlockHash {
    fn deserialize<D>(deserializer: D) -> Result<BlockHash, D::Error>
    where
        D: Deserializer<'de>,
    {
        let str = String::deserialize(deserializer)?;

        let bytes = hex::decode(str).map_err(|err| serde::de::Error::custom(err.to_string()))?;

        Ok(BlockHash(bytes))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Point {
    pub block_hash: BlockHash,
    pub block_slot: u64,
}

impl Default for Point {
    fn default() -> Self {
        Self {
            block_hash: BlockHash(Vec::with_capacity(0)),
            block_slot: 0,
        }
    }
}

impl Point {
    pub fn new(block_hash: BlockHash, block_slot: u64) -> Self {
        Point {
            block_hash,
            block_slot,
        }
    }
}

impl From<Point> for miniprotocols::Point {
    fn from(value: Point) -> Self {
        let Point {
            block_slot,
            block_hash: BlockHash(block_hash),
        } = value;
        miniprotocols::Point::new(block_slot, block_hash)
    }
}

impl From<miniprotocols::Point> for Point {
    fn from(value: miniprotocols::Point) -> Self {
        match value {
            miniprotocols::Point::Origin => Point {
                block_slot: 0,
                block_hash: BlockHash(Vec::with_capacity(0)),
            },
            miniprotocols::Point::Specific(block_slot, block_hash) => Point {
                block_slot,
                block_hash: BlockHash(block_hash),
            },
        }
    }
}
