#!/usr/bin/env bash
# reencode by Amory Meltzer
# Make smaller but visually pretty-good video files with ffmpeg

# Some helpful reading:
# <https://gist.github.com/arch1t3cht/b5b9552633567fa7658deee5aec60453/>
# <https://jaded-encoding-thaumaturgy.github.io/JET-guide/master/encoding/x264/>
# <https://dev.beandog.org/x264_preset_reference.html>
# <https://trac.ffmpeg.org/wiki/Encode/H.264>

# Some scattered examples:
# ffmpeg -hide_banner -loglevel warning -stats -i "${file}.mkv" -c:v h264_videotoolbox -b:v 3500k -maxrate 5250k -bufsize 7000k -profile:v high -level:v 4.0 -vf format=yuv420p -c:a aac_at -b:a 192k -c:s copy -map 0 "${file}-x264videotoolbox-35.mkv"
# ffmpeg -hide_banner -loglevel warning -stats -i "${file}.mkv" -c:v h264_videotoolbox -q:v 75 -profile:v high -level:v 4.0 -c:a copy -c:s copy -map 0 "${file}-x264videotoolbox-qv75.mkv"
# ffmpeg -hide_banner -loglevel warning -stats -i "${file}.mkv" -c:v hevc_videotoolbox -b:v 3500k -maxrate 5250k -bufsize 7000k -level:v 4.0 -vf format=yuv420p -c:a aac_at -b:a 192k -c:s copy -map 0 "${file}-hevcvideotoolbox-35.mkv"
# ffmpeg -hide_banner -loglevel warning -stats -i "${file}.mkv" -c:v libx265 -preset slow -crf 21 -c:a copy -c:s copy -map 0 "${file}-x265-21-slow.mkv"



for filename in *.mkv; do
    file="${filename%.*}"
    printf "\tProcessing %s\n" "$file";

    bitrate=$(ffprobe -i "${file}.mkv" -show_entries format=bit_rate -v quiet -of csv="p=0")
    if [ "$bitrate" -lt 4000000 ]; then
	echo "$file likely already compressed, bit rate is $bitrate"
	continue
    fi

    bitrate=$(ffprobe -v error -show_entries format=bit_rate -of csv=p=0 "$file.mkv")
    length=$(ffprobe -show_entries format=duration -sexagesimal -v quiet -of csv="p=0" "$file.mkv")

    echo "$file bit rate is $bitrate; length is $length"

    ffmpeg -hide_banner -loglevel warning -stats -i "${file}.mkv" -c:v libx264 -preset slow -crf 21 -vf format=yuv420p -c:a aac_at -b:a 192k -c:s copy -map 0 "${file}-x264.mkv"

    # Just in case?
    original_size=$(stat -f%z "${file}.mkv")
    new_size=$(stat -f%z "${file}-x264.mkv")
    if [ "$new_size" -ge "$original_size" ]; then
	echo "Warning: new file is larger than original, removing"
	rm "${file}-x264.mkv"
    fi


    printf "Finished processing %s\n\n" "$file"
done

echo "All files processed."
