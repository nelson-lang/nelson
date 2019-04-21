//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
"use strict";
//=============================================================================
const TableIDvalue = "indextable";
var TableLastSortedColumn = -1;
//=============================================================================
function SortTable() {
	const sortColumn = parseInt(arguments[0]);
  	const type = arguments[1].toUpperCase();
  	const table = document.getElementById(TableIDvalue);
  	const tbody = table.getElementsByTagName("tbody")[0];
  	const rows = tbody.getElementsByTagName("tr");
  	var arrayOfRows = new Array();

	for (var i = 0, len = rows.length; i < len; i++) {
    arrayOfRows[i] = new Object();
    arrayOfRows[i].oldIndex = i;
    var celltext = rows[i]
      .getElementsByTagName("td")
      [sortColumn].innerHTML.replace(/<[^>]*>/g, "");
    var re = type == "N" ? /[^\.\-\+\d]/g : /[^a-zA-Z0-9]/g;
    arrayOfRows[i].value = celltext
      .replace(re, "")
      .substr(0, 25)
      .toLowerCase();
  }

  if (sortColumn == TableLastSortedColumn) {
    arrayOfRows.reverse();
  } else {
    TableLastSortedColumn = sortColumn;
	if (type === 'N') {
		arrayOfRows.sort(CompareRowOfNumbers);
	} else {
		arrayOfRows.sort(CompareRowOfText);
	}
  }

  var newTableBody = document.createElement("tbody");

  for (var i = 0, len = arrayOfRows.length; i < len; i++) {
    newTableBody.appendChild(rows[arrayOfRows[i].oldIndex].cloneNode(true));
  }

  table.replaceChild(newTableBody, tbody);

  function CompareRowOfText(a, b) {
	var aval = a.value;
	var bval = b.value;
	return aval == bval ? 0 : aval > bval ? 1 : -1;
  }

  function CompareRowOfNumbers(a, b) {
	var aval = /\d/.test(a.value) ? parseFloat(a.value) : 0;
	var bval = /\d/.test(b.value) ? parseFloat(b.value) : 0;
	return aval == bval ? 0 : aval > bval ? 1 : -1;
  } 
} 
//=============================================================================
