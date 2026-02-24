CREATE SCHEMA IF NOT EXISTS public;

CREATE TABLE IF NOT EXISTS public.schools (
  school_code text PRIMARY KEY,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS public.pitch_data_files (
  file_id bigserial PRIMARY KEY,
  school_code text NOT NULL REFERENCES public.schools(school_code) ON DELETE CASCADE,
  source_file text NOT NULL,
  file_checksum text,
  file_mtime timestamptz,
  row_count integer,
  loaded_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (school_code, source_file)
);

CREATE TABLE IF NOT EXISTS public.pitch_events (
  id bigserial PRIMARY KEY,
  school_code text NOT NULL REFERENCES public.schools(school_code) ON DELETE CASCADE,
  file_id bigint NOT NULL REFERENCES public.pitch_data_files(file_id) ON DELETE CASCADE,
  session_date date,
  session_type text,
  source_file text,
  pitch_key text,
  Date text,
  Pitcher text,
  Email text,
  PitcherThrows text,
  TaggedPitchType text,
  InducedVertBreak text,
  HorzBreak text,
  RelSpeed text,
  ReleaseTilt text,
  BreakTilt text,
  SpinEfficiency text,
  SpinRate text,
  RelHeight text,
  RelSide text,
  Extension text,
  VertApprAngle text,
  HorzApprAngle text,
  PlateLocSide text,
  PlateLocHeight text,
  PitchCall text,
  KorBB text,
  Balls text,
  Strikes text,
  SessionType text,
  PlayID text,
  ExitSpeed text,
  Angle text,
  BatterSide text,
  PlayResult text,
  TaggedHitType text,
  OutsOnPlay text,
  Batter text,
  Catcher text,
  VideoClip text,
  VideoClip2 text,
  VideoClip3 text,
  PitchUID text,
  PitchID text,
  PitchGuid text,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_pitch_events_school_date
  ON public.pitch_events (school_code, session_date DESC, id DESC);

CREATE INDEX IF NOT EXISTS idx_pitch_events_school_pitcher_date
  ON public.pitch_events (school_code, Pitcher, session_date DESC);

CREATE INDEX IF NOT EXISTS idx_pitch_events_school_batter_date
  ON public.pitch_events (school_code, Batter, session_date DESC);

CREATE INDEX IF NOT EXISTS idx_pitch_events_school_catcher_date
  ON public.pitch_events (school_code, Catcher, session_date DESC);

CREATE INDEX IF NOT EXISTS idx_pitch_events_school_pitchtype_date
  ON public.pitch_events (school_code, TaggedPitchType, session_date DESC);

CREATE INDEX IF NOT EXISTS idx_pitch_events_pitch_key
  ON public.pitch_events (school_code, pitch_key);

CREATE INDEX IF NOT EXISTS idx_pitch_events_play_id
  ON public.pitch_events (PlayID);

CREATE INDEX IF NOT EXISTS idx_pitch_events_file_id
  ON public.pitch_events (file_id);

CREATE INDEX IF NOT EXISTS idx_pitch_events_date_brin
  ON public.pitch_events USING BRIN (session_date);

CREATE INDEX IF NOT EXISTS idx_pitch_events_video_partial
  ON public.pitch_events (school_code, session_date DESC)
  WHERE (coalesce(VideoClip, '') <> '' OR coalesce(VideoClip2, '') <> '' OR coalesce(VideoClip3, '') <> '');
