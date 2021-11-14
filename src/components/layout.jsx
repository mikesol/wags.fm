import React from "react";
import { globalCss } from "@stitches/react";
import { Helmet } from "react-helmet";
import Toplevel from "./toplevel";

const globalStyles = globalCss({
  html: {
    height: "100%",
    margin: 0,
    padding: 0,
    fontFamily:
      '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, "Noto Sans", sans-serif;',
  },
  body: { height: "100%", margin: 0, padding: 0, overflow: "hidden" },
  "#___gatsby": { height: "100%" },
  "#gatsby-focus-wrapper": { height: "100%" },
  "::-webkit-scrollbar": {
    width: "15px",
    height: "15px",
  },
});

export default function Layout({ children }) {
  globalStyles();
  return (
    <Toplevel>
      <Helmet>
        <meta charSet="utf-8" />
        <title>wags.fm</title>
        <link rel="canonical" href="https://wags.fm" />
        <script
          async
          src="https://www.googletagmanager.com/gtag/js?id=G-PTMVJD534Q"
        ></script>
        <script>
          {`window.dataLayer = window.dataLayer || []
          function gtag(){window.dataLayer.push(arguments)}
          gtag('js', new Date());
          gtag('config', 'G-PTMVJD534Q');`}
        </script>
      </Helmet>
      {children}
    </Toplevel>
  );
}
