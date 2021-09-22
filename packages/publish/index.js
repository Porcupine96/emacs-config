const serverUrl = "http://localhost:5000";

function indexVisible() {
  const visible = getQuery("indexVisible");
  return visible === undefined || visible == "true";
}

function toggleIndex() {
  if (!indexVisible()) {
    $("button.toggle-index").css("background-color", "#c1e1c1");
    $("div.index").css("display", "inline-block");
    setQuery("indexVisible", "true");
  } else {
    $("button.toggle-index").css("background-color", "#e58b88");
    $("div.index").css("display", "none");
    setQuery("indexVisible", "false");
  }
}

async function fetchNote(href) {
  const request = new Request(href);

  return await fetch(request).then((r) => r.text());
}

async function showIndex() {
  const response = await fetch(serverUrl + "/files", {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  });

  const files = await response.json();

  const indexPage = $("<div/>").addClass("index");
  const index = $("<ul/>").appendTo(indexPage);

  files.forEach((file) => {
    const li = $("<li/>").appendTo(index);
    $("<a/>")
      .attr("href", "/home/porcupine/kb/" + file)
      .text(file)
      .appendTo(li);
  });

  indexPage.appendTo("div.grid");

  overrideOnClick($("div.index")[0], 0);

  if (indexVisible()) {
    $("button.toggle-index").css("background-color", "#c1e1c1");
    $("div.index").css("display", "inline-block");
  } else {
    $("button.toggle-index").css("background-color", "#e58b88");
    $("div.index").css("display", "none");
  }
}

async function showPage(note, level) {
  const noteUrl = serverUrl + "/file/" + note;
  const content = await fetchNote(noteUrl);
  const old = $("div.grid")
    .children()
    .slice(level + 1);
  old.remove();

  const nextPage = $("<div class=page>");
  nextPage.append($(content).filter("#content")[0].outerHTML);
  $("div.grid").append(nextPage);

  overrideOnClick(nextPage[0], level + 1);
  fixImages();

  MathJax.Hub.Typeset();
}

async function showPages(query) {
  const notes = query["notes"].split(",");

  for (var level = 0; level < notes.length; level++) {
    const note = notes[level];
    const noteUrl = serverUrl + "/file/" + note;
    const content = await fetchNote(noteUrl);

    const nextPage = $("<div class=page>");
    nextPage.append($(content).filter("#content")[0].outerHTML);

    $("div.grid").append(nextPage);
  }

  // TODO: a temporary hack
  fixImages();

  const pages = document.getElementsByClassName("page");
  for (var level = 0; level < pages.length; level++) {
    overrideOnClick(pages[level], level + 1);
  }

  MathJax.Hub.Typeset();
}

function fixImages() {
  $("img").each(function () {
    const source = $(this).attr("src");
    $(this).attr("src", source.replace("./images/", serverUrl + "/image/"));
  });
}

function getQuery(key) {
  const url = new URL(window.location);
  const params = Object.fromEntries(url.searchParams.entries());
  return params[key];
}

function setQuery(key, value) {
  const url = new URL(window.location);
  const params = Object.fromEntries(url.searchParams.entries());
  params[key] = value;

  const paramList = Object.entries(params);

  let newQuery = `?${paramList[0][0]}=${paramList[0][1]}`;
  paramList.slice(1).map((p) => (newQuery += `&${p[0]}=${p[1]}`));

  const newUrl =
    window.location.protocol +
    "//" +
    window.location.host +
    window.location.pathname +
    newQuery;

  window.history.pushState({ path: newUrl }, "", newUrl);
}

function overrideOnClick(element, level) {
  const links = element.getElementsByTagName("a");

  for (const link of links) {
    link.dataset.level = level;

    if (!link.href.startsWith("http")) {
      link.onclick = function () {
        // TODO: refactor
        const location = link.href
          .replace(
            "file:///home/porcupine/.emacs.default/packages/publish/",
            ""
          )
          .replace("file:///home/porcupine/kb/", "");

        const note = location.split("#")[0];
        const level = parseInt(this.dataset.level);

        const notesString = getQuery("notes");
        const currentNotes = notesString
          ? notesString.split(",").slice(0, level)
          : [];

        const notes =
          currentNotes.length === 0
            ? note
            : currentNotes.join(",") + "," + note;

        setQuery("notes", notes);

        showPage(note, level);

        return false;
      };
    }
  }
}

window.onload = async function () {
  const url = new URL(window.location);
  const query = Object.fromEntries(url.searchParams.entries());

  if (query["notes"]) {
    await showIndex();
    await showPages(query);
  } else {
    await showIndex();
  }
};
