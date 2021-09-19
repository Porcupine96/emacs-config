const serverUrl = "http://localhost:5000";

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
}

function fixImages() {
  $("img").each(function () {
    const source = $(this).attr("src");
    $(this).attr("src", source.replace("./images/", serverUrl + "/image/"));
  });
}

function overrideOnClick(element, level) {
  const links = element.getElementsByTagName("a");

  for (const link of links) {
    link.dataset.level = level;
    link.onclick = function () {
      // TODO: refactor
      const location = link.href
        .replace("file:///home/porcupine/.emacs.default/packages/publish/", "")
        .replace("file:///home/porcupine/kb/", "");

      const note = location.split("#")[0];
      const level = this.dataset.level;

      const url = new URL(window.location);
      const urlQuery = Object.fromEntries(url.searchParams.entries());
      const currentNotes = urlQuery["notes"]
        ? urlQuery["notes"].split(",").slice(0, level)
        : [];

      const notes =
        currentNotes.length === 0 ? note : currentNotes.join(",") + "," + note;

      const query = new URLSearchParams(window.location.search);
      query.set("notes", notes);
      window.location.search = query;

      return false;
    };
  }
}

window.onload = async function () {
  const url = new URL(window.location);
  const query = Object.fromEntries(url.searchParams.entries());

  if (query["notes"]) {
    showPages(query);
  } else {
    showIndex();
  }
};
