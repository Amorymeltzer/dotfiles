# ffmpeg completion, from
# <https://github.com/momo0853/ffmpeg-bash-completion/blob/main/ffmpeg.sh>
# <https://github.com/clerk67/ffmpeg-completion/blob/master/ffmpeg>

_ffmpeg()
{
	local cur=${COMP_WORDS[$COMP_CWORD]}
	local prev=${COMP_WORDS[$COMP_CWORD-1]}

	# Helper function to split option and stream specifier
	_split_option() {
		if [[ $cur == *:* ]]; then
			local spec=${cur#*:}
			if [[ -z "$spec" ]]; then
				# If we have just the colon, suggest stream types
				COMPREPLY=( $(compgen -W "v a s" -- "") )
			fi
		fi
	}

	if [[ $cur == *:* ]]; then
		_split_option
		return
	fi

	if [ -n "$prev" ] && [ "${prev:0:1}" = "-" ]; then
		case "$prev" in
			-i)  # Input file
				COMPREPLY=( $(compgen -o filenames -- "$cur") ) ;;
			-ac|-a:c)
				COMPREPLY=( $(compgen -W "1 2 3 4 5 6 7 22" -- "$cur") ) ;;
			-filter|-filter:v|-filter:a)
				COMPREPLY=( $(compgen -W "$(ffmpeg -filters 2>/dev/null | sed -n '/-/,$p' | awk '{print $2}')" -- "$cur") ) ;;
			-af)
				COMPREPLY=( $(compgen -W "$(ffmpeg -filters 2>/dev/null | sed -n '/-/,$p' | grep -E 'A+-' -w | awk '{print $2}')" -- "$cur") ) ;;
			-vf)
				COMPREPLY=( $(compgen -W "$(ffmpeg -filters 2>/dev/null | sed -n '/-/,$p' | grep -E 'V+-' -w | awk '{print $2}')" -- "$cur") ) ;;
			-acodec|-c:a)
				COMPREPLY=( $(compgen -W "$(ffmpeg -encoders 2>/dev/null | sed -n '/--/,$p' | tail -n +2 | grep "^ A" | awk '{print $2}')" -- "$cur") ) ;;
			-vcodec|-c:v)
				COMPREPLY=( $(compgen -W "$(ffmpeg -encoders 2>/dev/null | sed -n '/--/,$p' | tail -n +2 | grep "^ V" | awk '{print $2}')" -- "$cur") ) ;;
			-scodec|-c:s)
				COMPREPLY=( $(compgen -W "$(ffmpeg -encoders 2>/dev/null | sed -n '/--/,$p' | tail -n +2 | grep "^ S" | awk '{print $2}') copy" -- "$cur") ) ;;
			-cpuflags)
				COMPREPLY=( $(compgen -W "3dnow 3dnowext altivec armv5te armv6 armv6t2 armv8 athlon athlonxp atom avx avx2 bmi1 bmi2 cmov fma3 fma4 k6 k62 k8 mmx mmxext neon pentium2 pentium3 pentium4 setend sse sse2 sse2slow sse3 sse3slow sse4.1 sse4.2 ssse3 vfp vfpv3 xop" -- "$cur") ) ;;
			-f)
				COMPREPLY=( $(compgen -W "$(ffmpeg -formats 2>/dev/null | sed -n '/--/,$p' | tail -n +2 | awk '{print $2}')" -- "$cur") ) ;;
			-c|-codec)
				COMPREPLY=( $(compgen -W "$(ffmpeg -codecs 2>/dev/null | sed -n '/--/,$p' | tail -n +2 | awk '{print $2}')" -- "$cur") ) ;;
			-discard)
				COMPREPLY=( $(compgen -W "none default noref bidir nokey all" -- "$cur") ) ;;
			-disposition)
				COMPREPLY=( $(compgen -W "default dub original comment lyrics karaoke forced hearing_impaired visual_impaired clean_effects attached_pic captions descriptions dependent metadata" -- "$cur") ) ;;
			-h|-\?|-help|--help)
				# Would be nice to complete encoders, etc. FIXME TODO
				COMPREPLY=( $(compgen -W "long full decoder= encoder= demuxer= muxer= filter= type= -L -version -muxers -demuxers -devices -decoders -encoders -filters -pix_fmts -layouts -sample_fmts" -- "$cur") ) ;;
			-hwaccel)
				COMPREPLY=( $(compgen -W "none auto $(ffmpeg -hwaccels 2>/dev/null | tail -n +2)" -- "$cur") ) ;;
			-init_hw_device)
				COMPREPLY=( $(compgen -W "cuda dxva2 vaapi vdpau qsv" -- "$cur") ) ;;
			-loglevel)
				COMPREPLY=( $(compgen -W "quiet panic fatal error warning info verbose debug trace" -- "$cur") ) ;;
			-movflags)
				COMPREPLY=( $(compgen -W "default_base_moof disable_chpl empty_moov faststart frag_custom frag_keyframe omit_tfhd_offset rtphint separate_moof" -- "$cur") ) ;;
			-preset)
				COMPREPLY=( $(compgen -W "ultrafast superfast veryfast faster fast medium slow slower veryslow placebo" -- "$cur") ) ;;
			-pix_fmt)
				COMPREPLY=( $(compgen -W "$(ffmpeg -pix_fmts 2>/dev/null | sed -n '/--/,$p' | tail -n +2 | awk '{print $2}')" -- "$cur") ) ;;
			-s)
				COMPREPLY=( $(compgen -W "320x240 640x480 1280x720 1920x1080 2560x1440 3840x2160 ntsc pal qntsc qpal sntsc spal film ntsc-film sqcif qcif cif 4cif 16cif qqvga qvga vga svga xga uxga qxga sxga qsxga hsxga wvga wxga wsxga wuxga woxga wqsxga wquxga whsxga whuxga cga ega hd480 hd720 hd1080 2k 2kflat 2kscope 4k 4kflat 4kscope nhd hqvga wqvga fwqvga hvga qhd 2kdci 4kdci uhd2160 uhd4320" -- "$cur") ) ;;
			-vprofile|-profile:v)
				COMPREPLY=( $(compgen -W "baseline main high high10 high422 high444" -- "$cur") ) ;;
			-r)
				COMPREPLY=( $(compgen -W "23.976 24 25 29.97 30 50 59.94 60" -- "$cur") ) ;;
			-ar)
				COMPREPLY=( $(compgen -W "8000 11025 16000 22050 32000 44100 48000 96000" -- "$cur") ) ;;
			-aspect)
				COMPREPLY=( $(compgen -W "4:3 16:9 1.3333 1.7777" -- "$cur") ) ;;
			-b|-b:v)
				COMPREPLY=( $(compgen -W "1M 2M 4M 8M 16M" -- "$cur") ) ;;
			-ab|-b:a)
				COMPREPLY=( $(compgen -W "64k 96k 128k 192k 256k 320k" -- "$cur") ) ;;
			-t|-to|-ss)
				# Generate common second values
				local seconds=()
				for i in 5 10 30 60 120 300 600 1800 3600; do
					seconds+=("$i")
				done

				# Generate HH:MM:SS patterns
				local timestamps=()
				for h in 00 01 02; do
					for m in 00 15 30 45; do
						for s in 10 15 30; do
							timestamps+=("$h:$m:$s")
							# timestamps+=("$h:$m:$s.000")
						done
					done
				done

				COMPREPLY=( $(compgen -W "${seconds[*]} ${timestamps[*]}" -- "$cur") ) ;;

			-metadata)
				COMPREPLY=( $(compgen -W "title= artist= album= year= track= genre= comment= language=" -- "$cur") ) ;;
			-map)
				COMPREPLY=( $(compgen -W "0:0 0:1 0:2 1:0 1:1" -- "$cur") ) ;;
		esac
	elif [ -n "$prev" ] && [ "$prev" = "help" ]; then
		COMPREPLY=( $(compgen -W "-buildconf -formats -muxers -demuxers -devices -codecs -decoders -encoders -bsfs -protocols -filters -pix_fmts -layouts -sample_fmts -colors -sources -sinks -hwaccels" -- "$cur") )
	elif [ -n "$cur" ] && [ "${cur:0:1}" = "-" ]; then
		local opts=()
		# Base options
		opts+=("-L -h -version -muxers -demuxers -devices -decoders -encoders -filters -pix_fmts -layouts -sample_fmts")
		opts+=("-v -y -n -stats -f -t -to -ss")
		# Options that support stream specifiers
		local stream_opts="-c -codec -filter -r -aspect -ar -ac -metadata -s -b"
		for opt in $stream_opts; do
			opts+=("$opt" "$opt:" "$opt:v" "$opt:a" "$opt:s")
		done
		# Regular options
		opts+=("-vn -vcodec -vf -aq -an -acodec -af -sn -scodec")
		opts+=(" $(ffmpeg -h full 2>/dev/null | grep -e "^-" -e "^  -" | awk '{print $1}')")
		COMPREPLY=( $(compgen -W "${opts[*]}" -- "$cur") )
	else
		COMPREPLY=( $(compgen -W "\- help" -- "$cur") )
	fi
}

complete -o default -F _ffmpeg ffmpeg
