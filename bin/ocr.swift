#!/usr/bin/env swift
// By Ethan Hahn, <https://evanhahn.com/mac-ocr-script/>
import Foundation
import Vision

func die(_ msg: String) -> Never {
  fputs("\(msg)\n", stderr)
  exit(1)
}

if CommandLine.arguments.count != 2 {
  die("usage: ocr /path/to/image1.jpg")
}

let path = URL(fileURLWithPath: CommandLine.arguments[1])

var recognizeTextRequest = RecognizeTextRequest()
recognizeTextRequest.automaticallyDetectsLanguage = true
recognizeTextRequest.usesLanguageCorrection = true
recognizeTextRequest.recognitionLevel = .accurate

guard let observations = try? await recognizeTextRequest.perform(on: path) else {
  die("couldn't recognize text")
}

for observation in observations {
  if let candidate = observation.topCandidates(1).first {
    print(candidate.string)
  }
}
