# Video Processing Folder

This folder is used for staging videos that will be automatically processed and uploaded to Cloudinary.

## 📁 Folder Structure

Create folders following this pattern:
```
video_processing/
├── videos_2025-10-15/
│   ├── VideoClip2/
│   │   ├── session_abc123_play1.mp4
│   │   └── session_abc123_play2.mp4
│   └── VideoClip3/
│       ├── session_def456_play1.mp4
│       └── session_def456_play2.mp4
└── videos_2025-10-16/
    └── VideoClip2/
        └── session_xyz789_play1.mp4
```

## 📱 How to Use

1. **Create date folder**: `videos_YYYY-MM-DD`
2. **Create camera folders**: `VideoClip2` (behind) or `VideoClip3` (side)
3. **Upload videos**: Named `session_<session-id>_<play-id>.mp4`
4. **Commit to GitHub**: Videos will be processed automatically
5. **Videos are deleted** from GitHub after successful upload

## 🎥 Video Naming Convention

- **Format**: `session_<session-id>_<play-id>.<extension>`
- **Example**: `session_d527cb2a-39c9-47b4-a25d-b58bc4772f10_pitch1.mp4`
- **Session ID**: From TrackMan or custom identifier
- **Play ID**: Your custom play identifier
- **Extensions**: `.mp4`, `.mov`, `.avi`, `.mkv`

## ⚡ Processing

Videos are automatically:
- ✅ Uploaded to Cloudinary
- ✅ Added to `data/video_map.csv`
- ✅ Deleted from GitHub
- ✅ Available in your Shiny app

## 🚫 Don't Use for VideoClip (Edger)

The `VideoClip` camera angle is handled automatically by the TrackMan API sync, so only use:
- `VideoClip2` - Behind/Center camera angles
- `VideoClip3` - Side/3rd base camera angles