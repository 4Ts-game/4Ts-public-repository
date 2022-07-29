/**
 *
 * Description:
 *
 **/

class Card{
	get inclusionTips() {
		return this._inclusionTips;
	}

	set inclusionTips(value) {
		this._inclusionTips = value;
	}
	get tableIndications() {
		return this._tableIndications;
	}

	set tableIndications(value) {
		this._tableIndications = value;
	}

	get type() {
		return this._type;
	}

	set type(value) {
		this._type = value;
	}
	get text() {
		return this._text;
	}

	set text(value) {
		this._text = value;
	}
	get title() {
		return this._title;
	}

	set title(value) {
		this._title = value;
	}
	get id() {
		return this._id;
	}

	set id(value) {
		this._id = value;
	}
	constructor(id,title,text,type,inclusionTips,tableIndications = null) {
		this._id = id;
		this._title = title;
		this._text = text;
		this._type = type;
		this._inclusionTips = inclusionTips;
		this._tableIndications = tableIndications == null ?  new TableIndications() : tableIndications;
	}

	cloneCard(newId=null){
		let c = new Card(newId, this.title, this.text, this.type, this.inclusionTips, this.tableIndications)
		return c;
	}

	setTableIndicationProperty(prop, val){
		this._tableIndications[prop] = val;
	}

	getTableIndicationProperty(prop){
		return this._tableIndications[prop];
	}
}

class TableIndications {
	get time3() {
		return this._time3;
	}

	set time3(value) {
		this._time3 = value;
	}
	get technology3() {
		return this._technology3;
	}

	set technology3(value) {
		this._technology3 = value;
	}
	get team3() {
		return this._team3;
	}

	set team3(value) {
		this._team3 = value;
	}
	get task3() {
		return this._task3;
	}

	set task3(value) {
		this._task3 = value;
	}
	get time2() {
		return this._time2;
	}

	set time2(value) {
		this._time2 = value;
	}
	get time1() {
		return this._time1;
	}

	set time1(value) {
		this._time1 = value;
	}
	get technology2() {
		return this._technology2;
	}

	set technology2(value) {
		this._technology2 = value;
	}
	get technology1() {
		return this._technology1;
	}

	set technology1(value) {
		this._technology1 = value;
	}
	get team2() {
		return this._team2;
	}

	set team2(value) {
		this._team2 = value;
	}
	get team1() {
		return this._team1;
	}

	set team1(value) {
		this._team1 = value;
	}
	get task2() {
		return this._task2;
	}

	set task2(value) {
		this._task2 = value;
	}
	get task1() {
		return this._task1;
	}

	set task1(value) {
		this._task1 = value;
	}

	constructor() {
		this._task1 = null;
		this._task2 = null;
		this._task3 = null;
		this._team1 = null;
		this._team2 = null;
		this._team3 = null;
		this._technology1 = null;
		this._technology2 = null;
		this._technology3 = null;
		this._time1 = null;
		this._time2 = null;
		this._time3 = null;
	}
}

export {Card};

