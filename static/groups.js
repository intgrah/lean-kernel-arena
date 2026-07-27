// @ts-check

/** @param {Element} body */
function toggle(body) {
  body.classList.toggle("open");
  const row = body.querySelector(".group-row");
  if (row)
    row.setAttribute("aria-expanded", String(body.classList.contains("open")));
}

for (const body of document.querySelectorAll("tbody.group")) {
  const row = body.querySelector(".group-row");
  if (!row) continue;
  row.setAttribute("role", "button");
  row.setAttribute("tabindex", "0");
  row.setAttribute("aria-expanded", "false");
  row.addEventListener("click", (event) => {
    const target = /** @type {Element | null} */ (event.target);
    if (target?.closest("a")) return;
    toggle(body);
  });
  row.addEventListener("keydown", (event) => {
    const key = /** @type {KeyboardEvent} */ (event).key;
    if (key === "Enter" || key === " ") {
      event.preventDefault();
      toggle(body);
    }
  });
}
