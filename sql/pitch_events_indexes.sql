-- Neon pitch event table and index baseline.
-- Run this in Neon SQL editor if you want to create the table manually.

CREATE TABLE IF NOT EXISTS pitch_events (
  ingest_key TEXT PRIMARY KEY,
  pitch_date DATE,
  source_file TEXT,
  ingested_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_pitch_events_pitch_date ON pitch_events (pitch_date);
CREATE INDEX IF NOT EXISTS idx_pitch_events_pitcher ON pitch_events ("Pitcher");
CREATE INDEX IF NOT EXISTS idx_pitch_events_batter ON pitch_events ("Batter");
CREATE INDEX IF NOT EXISTS idx_pitch_events_pitch_type ON pitch_events ("TaggedPitchType");
CREATE INDEX IF NOT EXISTS idx_pitch_events_play_id ON pitch_events ("PlayID");
CREATE INDEX IF NOT EXISTS idx_pitch_events_session_type ON pitch_events ("SessionType");
