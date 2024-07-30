CREATE TABLE sync_progress (
  id INT PRIMARY KEY DEFAULT 0,
  block_slot BIGINT NOT NULL, 
  block_hash BYTEA NOT NULL,
  CHECK (id = 0)
);
