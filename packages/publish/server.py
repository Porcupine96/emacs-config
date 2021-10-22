import os
from flask import Flask, jsonify, send_from_directory
from flask_cors import CORS

app = Flask(__name__)
app.config['CORS_HEADERS'] = 'Content-Type'
cors = CORS(app)

@app.route("/image/<path:path>")
def image(path):
    return send_from_directory('./images/', path)

@app.route("/file/<path:path>")
def file(path):
    return send_from_directory('.', path)

@app.route("/files")
def files():
    html_files = [p for p in os.listdir(".") if p.endswith(".html")]
    return jsonify(html_files)
