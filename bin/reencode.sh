#!/usr/bin/env bash
# reencode by Amory Meltzer
# Make smaller but visually pretty-good video files with ffmpeg, based on the
# specific bitrate: either keep, aim for a target, or cut in half

for filename in *.mkv; do
	file="${filename%.*}"
	printf "\n\tProcessing %s\n\n" "$file";

	bitrate=$(ffprobe -i "${file}.mkv" -show_entries format=bit_rate -v quiet -of csv="p=0")
	if [ "$bitrate" -lt 3500000 ]; then
		echo "$file likely small enough, bit rate is $bitrate"
	elif [ "$bitrate" -lt 6000000 ]; then
		echo "$file bit rate is $bitrate, targeting 3.5M"
		ffmpeg -i "${file}.mkv" -c:v h264_videotoolbox -b:v 3500k -maxrate 5250k -bufsize 7000k -profile:v high -level:v 4.0 -vf format=yuv420p -c:a aac_at -b:a 192k -c:s copy -map 0 "${file}-buf.mkv"
	else
		echo "$file bit rate is $bitrate, targeting half of that"
		newbitrate=$((bitrate / 2))
		bufsize=$bitrate
		maxrate=$((bitrate * 3/2))
		ffmpeg -i "${file}.mkv" -c:v h264_videotoolbox -b:v "$newbitrate" -maxrate "$maxrate" -bufsize "$bufsize" -profile:v high -level:v 4.0 -vf format=yuv420p -c:a aac_at -b:a 192k -c:s copy -map 0 "${file}-half.mkv"
	fi


	echo "Finished processing $file"
done

echo "All files processed."
