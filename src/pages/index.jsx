import React from "react";
import { styled } from "@stitches/react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  faChevronCircleDown,
  faChevronCircleUp,
} from "@fortawesome/free-solid-svg-icons";
import img from "../images/circuits.jpg";
import { useState, useEffect } from "react";
import { motion, AnimatePresence } from "framer-motion";
import { defineCustomElements as deckDeckGoHighlightElement } from "@deckdeckgo/highlight-code/dist/loader";
import loadingImg from "../images/wags.fm.transparent.png";

deckDeckGoHighlightElement();

const codeVariants = {
  starting: () => {
    return {
      x: "200%",
      opacity: 0,
    };
  },
  present: () => {
    return {
      x: 0,
      opacity: 1,
    };
  },
  absent: () => {
    return {
      opacity: 0,
      x: "-200%",
    };
  },
};

const mainVariants = {
  presentMain: () => {
    return {
      y: 0,
      opacity: 1,
      zIndex: 1,
    };
  },
  absentMain: ({ page }) => {
    return {
      zIndex: 0,
      y: page === 1 ? -1000 : 0,
      opacity: 0,
    };
  },
};

const LI = ({ ...props }) => (
  <motion.img
    {...props}
    animate={{ rotate: 360 }}
    transition={{ type: "spring", duration: 2.8, repeat: Infinity }}
  />
);

const Loading = ({ loading }) => (
  <AnimatePresence>
    {loading && (
      <motion.div
        exit={{ opacity: 0 }}
        style={{
          backgroundColor: "#8CD9D2",
          height: "100%",
          width: "100%",
          position: "absolute",
          zIndex: 2,
        }}
      >
        <FlexR>
          <Flex1></Flex1>
          <Flex0>
            <FlexC>
              <Flex1></Flex1>
              <Flex0>
                <div>
                  <LI width="100px" height="100px" src={loadingImg} />
                  <p
                    style={{
                      fontWeight: "600",
                      color: "#F55CA6",
                      textAlign: "center",
                    }}
                  >
                    ...loading...
                  </p>
                </div>
              </Flex0>
              <Flex1></Flex1>
            </FlexC>
          </Flex0>
          <Flex1></Flex1>
        </FlexR>
      </motion.div>
    )}
  </AnimatePresence>
);

const Motionable = ({ children, direction, page, variant, ...props }) => (
  <motion.div
    {...props}
    style={{ height: "100%", width: "100%", position: "absolute" }}
    animate={variant}
    variants={mainVariants}
    transition={{ duration: 0.6 }}
    custom={{ page, direction }}
  >
    {children}
  </motion.div>
);

const Listener = styled("div", {
  backgroundImage: `url(${img})`,
  backgroundRepeat: "no-repeat",
  backgroundPosition: "center",
  backgroundAttachment: "fixed",
  backgroundSize: "cover",
  height: "100%",
  width: "100%",
  display: "grid",
  gridTemplateColumns: "1fr 1fr 1fr",
  gridTemplateRows: "1fr 1fr 1fr",
});

const Coder = styled("div", {
  height: "100%",
  width: "100%",
});

const R3C3 = styled("div", {
  gridColumnStart: 3,
  gridColumnEnd: 3,
  gridRowStart: 3,
  gridRowEnd: 3,
});

const FlexC = styled("div", {
  width: "100%",
  height: "100%",
  alignContent: "space-between",
  display: "flex",
  flexDirection: "column",
});

const FlexR = styled("div", {
  width: "100%",
  height: "100%",
  alignContent: "space-between",
  display: "flex",
  flexDirection: "row",
});

const Flex1 = styled("div", {
  flexGrow: 1,
});

const Flex0 = styled("div", {
  flexGrow: 0,
});

const IconCase = styled("div", {
  cursor: "pointer",
  backgroundColor: "rgba(255,255,255,0.7)",
  padding: 10,
});

const MyFA = styled(FontAwesomeIcon, {});

const bindPage = (x) => (x >= 2 ? 0 : x < 0 ? 1 : x);

const Hello = () => {
  const [[page, direction], setPage] = useState([0, 0]);
  const paginate = (newDirection) => {
    setPage([bindPage(page + newDirection), newDirection]);
  };
  // snippet effect
  const [snippet, setSnippet] = useState(0);
  console.log("snippet", snippet);
  const incrementSnippet = () => (n) =>
    setTimeout(() => {
      setSnippet(n + 1);
      incrementSnippet()(n + 1);
    }, 4000);
  useEffect(() => {
    incrementSnippet()(snippet);
  }, []);
  // loadingEffect
  const [loading, setLoading] = useState(true);
  useEffect(() => {
    setTimeout(() => {
      setLoading(false);
    }, 4000);
  }, []);

  return (
    <>
      <Motionable
        key={0}
        page={page}
        direction={direction}
        variant={page === 0 ? "presentMain" : "absentMain"}
      >
        <Listener>
          <R3C3>
            <FlexC>
              <Flex1></Flex1>
              <Flex0>
                <FlexR>
                  <Flex1></Flex1>
                  <Flex0>
                    <IconCase onClick={() => paginate(1)}>
                      Edit me <MyFA icon={faChevronCircleDown}></MyFA>
                    </IconCase>
                  </Flex0>
                </FlexR>
              </Flex0>
            </FlexC>
          </R3C3>
        </Listener>
      </Motionable>
      <Motionable
        key={1}
        page={page}
        direction={direction}
        variant={page === 1 ? "presentMain" : "absentMain"}
      >
        <Coder>
          <FlexC>
            <Flex0>
              <FlexR>
                <Flex1></Flex1>
                <Flex0>
                  <IconCase onClick={() => paginate(-1)}>
                    Back <MyFA icon={faChevronCircleUp}></MyFA>
                  </IconCase>
                </Flex0>
              </FlexR>
            </Flex0>
            <Flex1>
              <FlexR>
                <Flex1></Flex1>
                <Flex1>
                  <div
                    style={{
                      position: "relative",
                      width: "100%",
                      height: "100%",
                    }}
                  >
                    <AnimatePresence>
                      {snippet % 3 === 0 && (
                        <motion.div
                          key={"code0"}
                          style={{ position: "absolute", width: "100%" }}
                          initial="starting"
                          animate="present"
                          exit="absent"
                          variants={codeVariants}
                          transition={{ duration: 1.2 }}
                        >
                          <deckgo-highlight-code
                            line-numbers={false}
                            editable={true}
                            language="haskell"
                          >
                            <code slot="code">
                              {`module HelloWorld where

a = 1 :: Int`}
                            </code>
                          </deckgo-highlight-code>
                        </motion.div>
                      )}
                      {snippet % 3 === 1 && (
                        <motion.div
                          key={"code1"}
                          style={{ position: "absolute", width: "100%" }}
                          initial="starting"
                          animate="present"
                          exit="absent"
                          variants={codeVariants}
                          transition={{ duration: 1.2 }}
                        >
                          <deckgo-highlight-code
                            line-numbers={false}
                            editable={true}
                            language="haskell"
                          >
                            <code slot="code">
                              {`module HelloWorld where

a = 2 :: Int`}
                            </code>
                          </deckgo-highlight-code>
                        </motion.div>
                      )}
                      {snippet % 3 === 2 && (
                        <motion.div
                          key={"code2"}
                          style={{ position: "absolute", width: "100%" }}
                          initial="starting"
                          animate="present"
                          exit="absent"
                          variants={codeVariants}
                          transition={{ duration: 1.2 }}
                        >
                          <deckgo-highlight-code
                            line-numbers={false}
                            editable={true}
                            language="haskell"
                          >
                            <code slot="code">
                              {`module HelloWorld where

a = 3 :: Int`}
                            </code>
                          </deckgo-highlight-code>
                        </motion.div>
                      )}
                    </AnimatePresence>
                  </div>
                </Flex1>
                <Flex1></Flex1>
              </FlexR>
            </Flex1>
          </FlexC>
        </Coder>
      </Motionable>
      <Loading loading={loading} />
    </>
  );
};

export default Hello;
