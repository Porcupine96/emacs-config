import sqlite3
from typing import Optional, List

from model import Node

DB_PATH = "/home/porcupine/.emacs.default/org-roam.db"


def get_node(node_id: str) -> Optional[Node]:
    db = sqlite3.connect(DB_PATH)
    cursor = db.execute(
        f"""SELECT id, file, title FROM nodes WHERE id='"{node_id}"';"""
    )
    doc = cursor.fetchone()

    if doc:
        node_id, path, title = doc
        return Node(
            node_id=__remove_quotation(node_id),
            path=__remove_quotation(path),
            title=__remove_quotation(title),
        )


def get_nodes(query: Optional[str] = None, limit: Optional[int] = None) -> List[Node]:
    db = sqlite3.connect(DB_PATH)

    where_fr = f"WHERE title LIKE '%{query}%'" if query else ""
    limit_fr = f"LIMIT {limit}" if limit else ""
    cursor = db.execute(f"""SELECT id, file, title FROM nodes {where_fr} {limit_fr};""")

    return [
        Node(
            node_id=__remove_quotation(node_id),
            path=__remove_quotation(path),
            title=__remove_quotation(title),
        )
        for node_id, path, title in cursor
    ]


def __remove_quotation(text: str) -> str:
    return text.strip().removeprefix('"').removesuffix('"').strip()
