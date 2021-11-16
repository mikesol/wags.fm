import React from "react";
import { styled } from "@stitches/react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  faPauseCircle,
  faPlayCircle,
  faChevronCircleDown,
  faChevronCircleUp,
} from "@fortawesome/free-solid-svg-icons";
import { Scrollbars } from "react-custom-scrollbars";
import img from "../images/circuits.jpg";
import { useState, useEffect } from "react";
import { motion, AnimatePresence } from "framer-motion";
import { defineCustomElements as deckDeckGoHighlightElement } from "@deckdeckgo/highlight-code/dist/loader";
import loadingImg from "../images/wags.fm.transparent.png";
import {
  pauseScroll,
  playScroll,
  initialSampleCache,
  playWags,
  stopWags as swag,
} from "../../output/WAGS.FM.Controller";

deckDeckGoHighlightElement();

const eff = (f) => (x) => () => f(x);

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

const long = `module WAGSI.Cookbook.LowFi where

import Prelude

import Data.Lens (_Just, set, traversed)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Math ((%))
import WAGS.Create.Optionals (highpass, pan)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Tidal (lnr, lnv, lvt, make, onTag, parse_, s)
import WAGSI.Plumbing.Types (WhatsNext)
import WAGS.Lib.Learn.Oscillator (lfo)

m2 = 4.0 * 1.0 * 60.0/111.0 :: Number

wag :: WhatsNext
wag =
  make (m2 * 2.0)
    { earth: s $ set (traversed <<< _Just <<< lnr) (lcmap unwrap \{ normalizedLittleCycleTime: t } -> 1.0 + t*0.1 ) $ parse_ "tink:1;t0 tink:2;t1 tink:3;t2 tink:0;t3 tink:4;t4 tink:2;t5 tink:3;t6 tink:1;t7 tink:2;t8 tink:0;t9 tink:3;t10 "
    , wind: map
        ( set lvt
            (lcmap unwrap \{ clockTime } -> let mody = clockTime % (m2 * 2.0) in fx
                ( goodbye $ highpass (200.0 + mody * 100.0) hello
                )
            )
        ) $ s $ onTag "ph" (set (_Just <<< lnr) $ lcmap unwrap \{ normalizedSampleTime: t } -> min 1.2 (1.0 + t*0.3) )
           $ onTag "print" (set (_Just <<< lnv) $ lcmap unwrap \{ normalizedSampleTime: _ } -> 0.2 )
           $ onTag "pk" (set (_Just <<< lnr) $ lcmap unwrap \{ normalizedSampleTime: t } -> 0.7 - t*0.2 )
           $ onTag "kt" (set (_Just <<< lnr) $ lcmap unwrap \{ normalizedSampleTime: t } -> min 1.0 (0.6 + t*0.8) ) $ parse_ "psr:3 ~ [~ chin*4] ~ ~ [psr:3;ph psr:3;ph ~ ] _ _ , [~ ~ ~ <psr:1;print kurt:0;print> ] kurt:5;kt , ~ ~ pluck:1;pk ~ ~ ~ ~ ~ "
    , fire: map
        ( set lvt
            (lcmap unwrap \{ clockTime } -> fx
                ( goodbye $ pan (lfo { phase: 0.0, amp: 1.0, freq: 0.2 } clockTime + 0.0) { myhp: highpass (lfo { phase: 0.0, amp: 2000.0, freq: 0.4 } clockTime + 2000.0) hello }
                )
            )
        ) $ s "~ ~ ~ ~ ~ ~ speechless:2 ~"
    , title: "lo fi"
    }

`;

const let_ = (a) => (f) => f(a);

const Hello = () => {
  const [[page, direction], setPage] = useState([0, 0]);
  const paginate = (newDirection) => {
    setPage([bindPage(page + newDirection), newDirection]);
  };
  // START ENGINE
  const [snippet, setSnippet] = useState(0);
  const [stopScrolling, setStopScrolling] = let_(
    useState({ stopScrolling: () => {} })
  )(([x, y]) => [x.stopScrolling, (stopScrolling) => y({ stopScrolling })]);
  const [isScrolling, setIsScrolling] = useState(false);
  const [isPlaying, setIsPlaying] = useState(false);
  const [stopWags, setStopWags] = let_(useState({ stopWags: () => {} }))(
    ([x, y]) => [x.stopWags, (stopWags) => y({ stopWags })]
  );
  const [newWagPush, setNewWagPush] = let_(
    useState({ newWagPush: () => () => {} })
  )(([x, y]) => [x.newWagPush, (newWagPush) => y({ newWagPush })]);
  const bufferCache = let_(useRef(initialSampleCache))((rf) => ({
    read: () => rf.current,
    write: (x) => () => {
      rf.current = x;
    },
  }));
  // END ENGINE
  // START FNs
  const playlist = [];
  const scrollPauser = pauseScroll({
    stopScrolling,
    setIsScrolling: eff(setIsScrolling),
    setStopScrolling: eff(setStopScrolling),
  });
  const wagsStopper = swag({
    stopScrolling,
    setStopScrolling: eff(setStopScrolling),
    setIsScrolling: eff(setIsScrolling),
    setIsPlaying: eff(setIsPlaying),
    stopWags,
    setStopWags: eff(setStopWags),
  });
  const scrollPlayer = playScroll({
    playlist,
    newWagPush,
    snippet,
    setStopScrolling: eff(setStopScrolling),
    setSnippet: eff(setSnippet),
    isScrolling,
    setIsScrolling: eff(setIsScrolling),
  });
  const wagsPlayer = playWags({
    snippet,
    stopScrolling,
    setStopScrolling: eff(setStopScrolling),
    setSnippet: eff(setSnippet),
    setIsScrolling: eff(setIsScrolling),
    isPlaying: isPlaying,
    setIsPlaying: eff(setIsPlaying),
    setStopWags: eff(setStopWags),
    bufferCache,
    playlist,
  });
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
                      overflowY: "scroll",
                      scrollbarWidth: "none",
                    }}
                  >
                    <AnimatePresence>
                      {snippet % 2 === 0 && (
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
                            onClick={scrollPauser}
                            line-numbers={false}
                            editable={true}
                            language="haskell"
                          >
                            <code slot="code">{long}</code>
                          </deckgo-highlight-code>
                        </motion.div>
                      )}
                      {snippet % 2 === 1 && (
                        <motion.div
                          key={"code1"}
                          style={{
                            position: "absolute",
                            width: "100%",
                          }}
                          initial="starting"
                          animate="present"
                          exit="absent"
                          variants={codeVariants}
                          transition={{ duration: 1.2 }}
                        >
                          <deckgo-highlight-code
                            onClick={scrollPauser}
                            line-numbers={false}
                            editable={true}
                            language="haskell"
                          >
                            <code slot="code">{long}</code>
                          </deckgo-highlight-code>
                        </motion.div>
                      )}
                    </AnimatePresence>
                  </div>
                </Flex1>
                <Flex1></Flex1>
              </FlexR>
            </Flex1>
            <div
              style={{
                height: "10%",
                display: "flex",
                flexDirection: "row",
                alignContent: "space-between",
              }}
            >
              <Flex1></Flex1>
              <Flex0>
                <FontAwesomeIcon
                  size={"2x"}
                  style={{ paddingTop: "10px", cursor: "pointer" }}
                  onClick={isScrolling ? scrollPauser : scrollPlayer}
                  icon={isScrolling ? faPauseCircle : faPlayCircle}
                ></FontAwesomeIcon>
              </Flex0>
              <Flex1></Flex1>
            </div>
          </FlexC>
        </Coder>
      </Motionable>
      <Loading loading={loading} />
    </>
  );
};

export default Hello;
