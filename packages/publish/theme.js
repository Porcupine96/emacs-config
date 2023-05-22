async function fetchNote(href) {
  const request = new Request(href);

  return await fetch(request).then((r) => r.text());
}

// function overrideOnClick() {
//   $(document).ready(function () {
//     const pages = document.getElementsByClassName("page");
//     for (var level = 0; level < pages.length; level++) {
//       const page = pages[level];
//       const links = page.getElementsByTagName("a");
//       for (const link of links) {
//         link.dataset.level = level;
//         link.onclick = function () {
//           // TODO: refactor
//           const location = link.href.replace("file:///home/porcupine/kb/", "");
//           const note = location.split("#")[0];
//           const level = this.dataset.level;

//           const nodes =
//             currentNodes.length === 0
//               ? note
//               : currentNodes.join(",") + "," + note;

//           const query = new URLSearchParams(window.location.search);
//           query.set("nodes", nodes);
//           window.location.search = query;

//           return false;
//         };
//       }
//     }
//   });
// }

// window.onload = async function () {
//   $(document).ready(function () {
//     $("body")
//       .wrapInner("<div class=grid><div class=page></div></div>")
//       .prepend(
//         "<div class=header onclick=window.location='/home/porcupine/kb/index.html'></div>"
//       );
//   });

//   const url = new URL(window.location);
//   const query = Object.fromEntries(url.searchParams.entries());

//   if (query["nodes"]) {
//     for (const node of query["nodes"].split(",")) {
//       const nodeUrl = "http://localhost:8000/" + node;
//       const content = await fetchNote(nodeUrl);

//       const nextPage = $("<div class=page>");
//       nextPage.append($(content).filter("#content")[0].outerHTML);

//       $("div.grid").append(nextPage);
//     }
//   }

//   overrideOnClick();
// };

// window.addEventListener("popstate", function (event) {
//   window.location = window.location;
// });
