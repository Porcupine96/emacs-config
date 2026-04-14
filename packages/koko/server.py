# /// script
# requires-python = ">=3.10,<3.13"
# dependencies = [
#     "kokoro>=0.9.4",
#     "soundfile",
#     "flask",
#     "numpy",
# ]
# ///

import os
os.environ["PYTORCH_ENABLE_MPS_FALLBACK"] = "1"

import io

import numpy as np
import soundfile as sf
from flask import Flask, Response, jsonify, request
from kokoro import KPipeline

app = Flask(__name__)

print("Loading Kokoro model...")
pipeline = KPipeline(lang_code="a")
print("Model loaded.")

SAMPLE_RATE = 24000
DEFAULT_VOICE = "af_heart"
DEFAULT_SPEED = 1.0


@app.route("/health", methods=["GET"])
def health():
    return jsonify({"status": "ok"})


@app.route("/tts", methods=["POST"])
def tts():
    data = request.get_json(force=True)
    text = data.get("text", "").strip()
    if not text:
        return jsonify({"error": "text is required"}), 400

    voice = data.get("voice", DEFAULT_VOICE)
    speed = float(data.get("speed", DEFAULT_SPEED))

    try:
        audio_chunks = []
        for _graphemes, _phonemes, audio in pipeline(text, voice=voice, speed=speed):
            if audio is not None:
                audio_chunks.append(audio)

        if not audio_chunks:
            return jsonify({"error": "no audio generated"}), 500

        full_audio = np.concatenate(audio_chunks)
        buf = io.BytesIO()
        sf.write(buf, full_audio, SAMPLE_RATE, format="WAV")
        buf.seek(0)
        return Response(buf.read(), mimetype="audio/wav")
    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == "__main__":
    app.run(host="127.0.0.1", port=5001)
