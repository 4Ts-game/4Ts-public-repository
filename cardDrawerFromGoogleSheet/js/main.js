/**
 *
 * Description:
 *
 **/

import {SheetToJSON} from "../classes/SheetToJSON.js";
import {Card} from "../classes/card.js";

let Cards = [];

function drawCards() {
	// loop cards and create a clean array of cards
	for (const [key, value] of Object.entries(SheetToJSONClass.json)) {

		let card = new Card();

		if (value.Type === "Wildcard" || value.Type === "Question Mark")
			continue;

		if (value.Type == "Technique")
			for (const [key, val] of Object.entries(value)) {

				switch (key.toUpperCase()) {

					case "TABLE INDICATIONS TASK1":
						card.setTableIndicationProperty('_task1', val);
						break;

					case "TABLE INDICATIONS TASK2":
						card.setTableIndicationProperty('_task2', val);
						break;

					case "TABLE INDICATIONS TASK3":
						card.setTableIndicationProperty('_task3', val);
						break;

					case "TABLE INDICATIONS TEAM1":
						card.setTableIndicationProperty('_team1', val);
						break;

					case "TABLE INDICATIONS TEAM2":
						card.setTableIndicationProperty('_team2', val);
						break;

					case "TABLE INDICATIONS TEAM3":
						card.setTableIndicationProperty('_team3', val);
						break;

					case "TABLE INDICATIONS TECHNOLOGY1":
						card.setTableIndicationProperty('_technology1', val);
						break;

					case "TABLE INDICATIONS TECHNOLOGY2":
						card.setTableIndicationProperty('_technology2', val);
						break;

					case "TABLE INDICATIONS TECHNOLOGY3":
						card.setTableIndicationProperty('_technology3', val);
						break;

					case "TABLE INDICATIONS TIME1":
						card.setTableIndicationProperty('_time1', val);
						break;

					case "TABLE INDICATIONS TIME2":
						card.setTableIndicationProperty('_time2', val);
						break;

					case "TABLE INDICATIONS TIME3":
						card.setTableIndicationProperty('_time3', val);
						break;
				}
			}

		for (const [key, val] of Object.entries(value)) {
			switch (key.toUpperCase()) {

				case "TYPE":
					card.type = val;
					break;

				case "TITLE":
					card.title = val;
					break;

				case "DESCRIPTION":
					card.text = val;
					break;

				case "INCLUSION_TIPS":
					card.inclusionTips = val;
					break;

				case "CARD 1 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 2 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 3 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 4 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 5 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 6 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 7 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 8 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 9 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 10 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
				case "CARD 11 ID":
					if (val != "") {
						Cards.push(card.cloneCard(val));
					}
					break;
			}
		}
	}

	//order cards by type
	Cards.sort((a, b) => (parseFloat(a._type) > parseFloat(b._type)) ? -1 : 1);

	$("#header").remove();
	let title = $("<div id='header'><h1> Pleiade i4T Cards</h1><h3> Cards generated from the Google Sheet ID: <b>" + $("#sheetData input").val() + "</b></h3></div>");
	$("#title").append(title);

	let cardType = "none";
	Cards.forEach((card) => {

		// console.debug(card);

		card.isJolly = false;
		if (card._title.length === 0)
			card.isJolly = true;

		// console.debug(card.isJolly);

		if (cardType != card._type) {

			let sectionHeader = $("<div>").addClass("section-header");

			sectionHeader.append("<br>")
			             .append("<h2>" + card._type + "</h2>")
			             .append("<hr>")
			             .append("<br>");

			cardType = card._type;

			$("#cards").append(sectionHeader);
		}

		let cardObj = $("<div/>");
		cardObj.attr({id: "card_" + card._id});
		cardObj.addClass("card " + card._type);
		cardObj.addClass(card.isJolly ? "jolly" : "");
		console.debug(card._id)
		cardObj.addClass(card._id == 19 ? "suggestion" : "");

		cardObj.append('<p class="id">' + card._id + '</p>');

		let frontObject = $('<div>').addClass("front");
		cardObj.append(frontObject);
		frontObject.append('<p class="marker"><img src="markers/4x4_1000-' + card._id + '.svg"></p>');

		let titleBox = $("<div/>").addClass("titleBox");


		titleBox.append('<p class="type">' + card._type + '</p>');
		titleBox.append('<p class="title">' + card._title + '</p>');
		frontObject.append(titleBox);

		let text = card._text.replace("\n", "<br>");
		frontObject.append('<p class="description">' + text + '</p>');

		let backObject = $('<div>').addClass("back");
		if (card._inclusionTips != null && card._inclusionTips.length > 0) {

			text = card._inclusionTips.replace("\n", "<br>");
			backObject.append(titleBox.clone());
			backObject.append('<p class="inclusionTips">' + text + '</p>');
		}
		cardObj.append(backObject);

		/*
				if (cardType == "Technique") {
					frontObject.append('<p class="description"><span class="cardType">' + card._type + '</span> - ' + card._text + '</p>');
					backObject.append('<p class="inclusionTips">' + card._inclusionTips + '</p>');

				} else {
					backObject.append('<p class="description"><span class="cardType">' + card._type + '</span> - ' + card._text + '</p>')
					backObject.append('<p class="inclusionTips">' + card._inclusionTips + '</p>');
				}
		*/
		/*
				if (cardType == "Technique") {

					let table = $("<table/>").addClass("techniqueTable");
					//TASK
					let tableRow = $("<tr>").addClass("taskRow");

					let cell = $("<td/>").addClass("icon").html('<span class="icon-task"></span>');
					tableRow.append(cell);

					cell = $("<td/>").addClass("task1").html(card.tableIndications.task1);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task2").html(card.tableIndications.task2);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task3").html(card.tableIndications.task3);
					tableRow.append(cell);

					table.append(tableRow);

					//TEAM
					tableRow = $("<tr>").addClass("teamRow");

					cell = $("<td/>").addClass("icon").html('<span class="icon-team"></span>');
					tableRow.append(cell);

					cell = $("<td/>").addClass("task1").html(card.tableIndications.team1);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task2").html(card.tableIndications.team2);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task3").html(card.tableIndications.team3);
					tableRow.append(cell);

					table.append(tableRow);

					//TECHNOLOGY
					tableRow = $("<tr>").addClass("technologyRow");

					cell = $("<td/>").addClass("icon").html('<span class="icon-technology"></span>');
					tableRow.append(cell);

					cell = $("<td/>").addClass("task1").html(card.tableIndications.technology1);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task2").html(card.tableIndications.technology2);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task3").html(card.tableIndications.technology3);
					tableRow.append(cell);

					table.append(tableRow);

					//TIME
					tableRow = $("<tr>").addClass("timeRow");

					cell = $("<td/>").addClass("icon").html('<span class="icon-time"></span>');
					tableRow.append(cell);

					cell = $("<td/>").addClass("task1").html(card.tableIndications.time1);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task2").html(card.tableIndications.time2);
					tableRow.append(cell);

					cell = $("<td/>").addClass("task3").html(card.tableIndications.time3);
					tableRow.append(cell);

					table.append(tableRow);
					//backObject.append(table);

				}
		*/

		$("#cards").append(cardObj);

	})

}

window.newSheetToJSONClass = function (sheetID, gridId = 0) {

	$("#cards").empty();
	Cards = [];
	window.SheetToJSONClass = new SheetToJSON(sheetID, gridId);
	window.SheetToJSONClass.ready.done(drawCards);
};
