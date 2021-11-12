module.exports = {
  pathPrefix: `/wags.fm`,
  siteMetadata: {
    siteUrl: "https://wags.fm",
    title: "wags.fm",
  },
  plugins: [
    "gatsby-plugin-netlify-cms",
    "gatsby-plugin-image",
    "gatsby-plugin-fontawesome-css",
    "gatsby-plugin-react-helmet",
    "gatsby-plugin-sitemap",
    {
      resolve: "gatsby-plugin-manifest",
      options: {
        icon: "src/images/icon.jpeg",
      },
    },
    "gatsby-plugin-sharp",
    "gatsby-transformer-sharp",
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "images",
        path: "./src/images/",
      },
      __key: "images",
    },
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "pages",
        path: "./src/pages/",
      },
      __key: "pages",
    },
  ],
};
