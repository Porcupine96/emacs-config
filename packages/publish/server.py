import os
from flask import Flask, jsonify, send_from_directory, request
from flask_cors import CORS
from db import get_nodes, get_node

app = Flask(__name__)
app.config["CORS_HEADERS"] = "Content-Type"
cors = CORS(app)


@app.route("/image/<path:path>")
def image(path):
    return send_from_directory("./images/", path)


@app.route("/file/<path:path>")
def file(path):
    return send_from_directory(".", path)


@app.route("/node-html/<string:node_id>")
def node_html(node_id: str):
    node = get_node(node_id)
    if node:
        fname = os.path.basename(node.path).replace("org", "html")
        return send_from_directory(".", fname)
    else:
        return "", 404


@app.route("/nodes")
def nodes():
    limitQuery = request.args.get("limit")
    limit = int(limitQuery) if limitQuery else None
    return jsonify(
        [
            {
                "nodeId": n.node_id,
                "title": n.title,
            }
            for n in get_nodes(query=request.args.get("query"), limit=limit)
        ]
    )


@app.route("/files")
def files():
    html_files = [p for p in os.listdir(".") if p.endswith(".html")]
    return jsonify(html_files)
