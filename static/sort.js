// @ts-check

const collator = new Intl.Collator(undefined, {
  numeric: true,
  sensitivity: "base",
});

/**
 * @param {HTMLTableRowElement} row
 * @param {number} index
 * @returns {string | number}
 */
function key(row, index) {
  const cell = row.cells[index];
  if (!cell) return "";
  const raw = (cell.dataset.sort ?? cell.textContent ?? "").trim();
  const value = Number(raw);
  return raw !== "" && !Number.isNaN(value) ? value : raw;
}

/**
 * @param {HTMLTableRowElement} a
 * @param {HTMLTableRowElement} b
 * @param {number} index
 * @param {number} direction
 */
function compare(a, b, index, direction) {
  const left = key(a, index);
  const right = key(b, index);
  if (typeof left === "number" && typeof right === "number") {
    return (left - right) * direction;
  }
  if (typeof left === "number") return -1;
  if (typeof right === "number") return 1;
  return collator.compare(left, right) * direction;
}

/**
 * @param {HTMLTableElement} table
 * @param {number} index
 * @param {number} direction
 */
function sort(table, index, direction) {
  const bodies = /** @type {HTMLTableSectionElement[]} */ ([...table.tBodies]);
  for (const body of bodies) {
    [...body.rows]
      .filter((row) => !row.classList.contains("group-row"))
      .sort((a, b) => compare(a, b, index, direction))
      .forEach((row) => body.append(row));
  }
  bodies
    .sort((a, b) => {
      const first = a.rows[0];
      const second = b.rows[0];
      if (!first || !second) return 0;
      return compare(first, second, index, direction);
    })
    .forEach((body) => table.append(body));
}

for (const table of document.querySelectorAll("table.arena-table")) {
  const headers = /** @type {HTMLTableCellElement[]} */ ([
    ...table.querySelectorAll("th.sortable"),
  ]);
  for (const header of headers) {
    header.setAttribute("role", "button");
    header.setAttribute("tabindex", "0");
    const activate = () => {
      const direction =
        header.getAttribute("aria-sort") === "ascending" ? -1 : 1;
      for (const other of headers) other.removeAttribute("aria-sort");
      header.setAttribute(
        "aria-sort",
        direction === 1 ? "ascending" : "descending",
      );
      sort(/** @type {HTMLTableElement} */ (table), header.cellIndex, direction);
    };
    header.addEventListener("click", activate);
    header.addEventListener("keydown", (event) => {
      const pressed = /** @type {KeyboardEvent} */ (event).key;
      if (pressed === "Enter" || pressed === " ") {
        event.preventDefault();
        activate();
      }
    });
  }
}
