CREATE TABLE sync_progress (
  block_slot BIGINT NOT NULL, 
  block_hash BYTEA NOT NULL,
  processed BOOL PRIMARY KEY
);
