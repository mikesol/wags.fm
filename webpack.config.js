var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = (env) => {
	var base = {
		mode: env.production ? "production" : "development",
		entry: process.env.WEBPACK_ENTRY
			? process.env.WEBPACK_ENTRY
			: "./src/index.js",
		output: {
			path: path.resolve(__dirname, "dist"),
			filename: "bundle.js",
			clean: true,
		},
		plugins: [
			new HtmlWebpackPlugin({
				favicon: "./src/images/wags.fm.png",
				template: "index.html",
			}),
		],
		module: {
			rules: [
				{
					test: /\.js$/i,
					include: path.resolve(__dirname, "src"),
					use: {
						loader: "babel-loader",
						options: {
							presets: ["@babel/preset-env"],
						},
					},
				},
				{
					test: /\.(png|svg|jpg|jpeg|gif)$/i,
					type: "asset/resource",
				},
				{
					test: /\.purs$/i,
					include: path.resolve(__dirname, "src"),
					type: "asset/source",
				},
				{
					test: /\.css$/i,
					include: path.resolve(__dirname, "src"),
					use: ["style-loader", "css-loader", "postcss-loader"],
				},
			],
		},
		devServer: {
			contentBase: path.resolve(__dirname, "dist"),
			watchContentBase: true,
			disableHostCheck: true,
			hot: true,
		},
	};
	return Object.assign(
		env.production
			? {}
			: {
					resolve: {
						alias: {
							"../Halogen.Component/index.js":
								"../../src/Halogen.Component.patch.js",
						},
					},
			  },
		base
	);
};
