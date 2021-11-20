var path = require("path");
var webpack = require("webpack");

module.exports = {
	mode: "production",
	entry: process.env.WEBPACK_ENTRY
		? process.env.WEBPACK_ENTRY
		: "./src/index.js",
	output: {
		path: path.resolve(__dirname, "dist"),
		filename: "bundle.js",
	},
	plugins: [new webpack.EnvironmentPlugin({ WAGSI_MODE: "live" })],
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
				test: /(Example|Wagged)\.purs/,
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
