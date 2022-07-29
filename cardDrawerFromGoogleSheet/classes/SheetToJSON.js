/**
 *
 * Description:
 *
 * id = 1oMOZyhZ5EdQYDa4lDQylRIFQWmZ1ZpvGFc8vxQm73Mw
 **/

class SheetToJSON {
	get gridId() {
		return this._gridId;
	}

	set gridId(value) {
		this._gridId = value;
	}
	get json() {
		return this._json;
	}

	set json(value) {
		this._json = value;
	}
	get url() {
		return this._url;
	}

	set url(value) {
		this._url = value;
	}
	get id() {
		return this._id;
	}

	set id(value) {
		this._id = value;
	}

	constructor(id, gridId = 0) {
		this._id = id;
		this._gridId = gridId;
		this._url = null;
		this._json = null;

		this.readyDeferred = new $.Deferred();
		this.ready = this.readyDeferred;

		this.loadCSV();
	}

	loadCSV(){
		//https://docs.google.com/spreadsheets/d/1oMOZyhZ5EdQYDa4lDQylRIFQWmZ1ZpvGFc8vxQm73Mw/export?format=csv&gid=0
		this._url = 'https://docs.google.com/spreadsheets/d/' + this.id + "/export?format=tsv&gid=" + this.gridId;

		let cvs = this;

		$.ajax({
			url: this._url,
			dataType: "text",
			contentType: "csv",
			success: function(content){
				cvs.json = csvJSON(content);
				cvs.readyDeferred.resolve();
				//console.debug(cvs.json);
			},
			error: function (error) {
				console.debug(error);
				alert("The Google Sheet ID is not valid. Check it and try again.");
			}
		})

		//var csv is the TSV file with headers
		function csvJSON(tsv){

			var lines=tsv.split("\n");

			var result = [];

			// NOTE: If your columns contain commas in their values, you'll need
			// to deal with those before doing the next step
			// (you might convert them to &&& or something, then covert them back later)
			// jsfiddle showing the issue https://jsfiddle.net/
			var headers=lines[0].split("\t");

			for(var i=1;i<lines.length;i++){

				var obj = {};
				var currentline=lines[i].split("\t");

				for(var j=0;j<headers.length;j++){
					obj[headers[j]] = currentline[j];
				}

				result.push(obj);

			}

			return result; //JavaScript object
			// return JSON.stringify(result); //JSON
		}
	}

}

export {SheetToJSON};

