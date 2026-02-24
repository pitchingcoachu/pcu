# Neon Pitch Data Migration

## What this adds

- Neon/Postgres backend for `pitch_data` with automatic CSV fallback.
- Chunked keyset pagination for large reads.
- Parallel monthly partition reads (configurable workers).
- File cache for startup acceleration.
- Indexed schema with foreign keys and BRIN support for time-series scale.
- CSV -> Neon sync script and optional automatic sync after FTP pulls.

## Environment variables

Set these in your runtime environment:

- `PITCH_DATA_BACKEND=postgres`
- `PITCH_DATA_DB_URL=postgres://...` (preferred)
- `PITCH_DATA_DB_SCHEMA=public` (optional)
- `PITCH_DATA_DB_TABLE=pitch_events` (optional)
- `PITCH_DATA_CACHE_TTL_SEC=900` (optional)
- `PITCH_DATA_PARALLEL_WORKERS=2` (optional)
- `PITCH_DATA_CHUNK_SIZE=100000` (optional)

Optional sync settings:

- `PITCH_DATA_SYNC_AFTER_FTP=1`
- `PITCH_DATA_SYNC_WORKERS=2`

## Initial migration

1. Create schema/tables/indexes:

```bash
Rscript -e 'source("pitch_data_service.R"); ensure_pitch_data_schema()'
```

2. Load CSV history into Neon:

```bash
Rscript scripts/sync_pitch_data_to_neon.R data 2
```

3. Start app with Neon backend enabled:

```bash
PITCH_DATA_BACKEND=postgres R -e 'shiny::runApp()'
```

## Notes

- If Neon is unreachable or misconfigured, app falls back to local CSV ingestion.
- Cache files are written to `PITCH_DATA_CACHE_DIR` (default `/tmp`).
- Existing downstream app logic remains intact to reduce migration risk.
