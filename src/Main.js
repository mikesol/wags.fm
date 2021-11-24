exports.hackishlyRemoveInitialSSR = function () {
	var element = document.getElementById("hackish-ssr-div");
	if (element) {
		element.parentNode.removeChild(element);
	}
};
