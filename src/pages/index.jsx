import React from "react";
import { styled } from "@stitches/react";
import { useAlert } from "react-alert";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
	faPauseCircle,
	faStopCircle,
	faPlayCircle,
	faChevronCircleDown,
	faChevronCircleUp,
} from "@fortawesome/free-solid-svg-icons";
import img from "../images/circuits.jpg";
import { useState, useEffect, useRef } from "react";
import { motion, AnimatePresence } from "framer-motion";
import { playlist as simple } from "../playlists/simple";
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

const editMeVariants = {
	starting: () => {
		return {
			opacity: 0,
		};
	},
	present: () => {
		return {
			opacity: 1,
		};
	},
	absent: () => {
		return {
			opacity: 0,
		};
	},
};
const errorScreenVariants = {
	starting: () => {
		return {
			opacity: 0,
		};
	},
	present: () => {
		return {
			opacity: 1,
		};
	},
	absent: () => {
		return {
			opacity: 0,
		};
	},
};

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
									<LI width='100px' height='100px' src={loadingImg} />
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

const R2C2 = styled("div", {
	gridColumnStart: 2,
	gridColumnEnd: 2,
	gridRowStart: 2,
	gridRowEnd: 2,
});

const R1C1 = styled("div", {
	gridColumnStart: 1,
	gridColumnEnd: 1,
	gridRowStart: 1,
	gridRowEnd: 3,
});

const FlexC = styled("div", {
	width: "100%",
	height: "100%",
	alignContent: "space-between",
	display: "flex",
	flexDirection: "column",
});

const WagsFM = styled("h1", {
	fontSize: "2em",
	fontWeight: "600",
	backgroundColor: "rgba(255,255,255,0.7)",
});

const WagsUL = styled("ul", {
	listStyle: "none",
	paddingLeft: "0px",
});
const WagsNowPlaying = styled("h2", {
	textAlign: "center",
	padding: "5px",
	backgroundColor: "rgba(255,255,255,0.7)",
	fontSize: "1.5em",
	fontWeight: "400",
	margin: "10px",
});
const WagsLI = styled("li", {
	backgroundColor: "rgba(255,255,255,0.7)",
	marginBottom: "10px",
});
const WagsLink = styled("a", {
	textDecoration: "none",
	color: "#1d7484",
	"&:hover": {
		color: "#982c61",
		cursor: "pointer",
		borderBottom: "2px solid #4a4a4a",
	},
	"&:visited": {
		color: "#144f5a",
	},
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
	textAlign: "center",
	cursor: "pointer",
	backgroundColor: "rgba(255,255,255,0.7)",
	padding: 10,
});

const bindPage = (x) => (x >= 2 ? 0 : x < 0 ? 1 : x);

const let_ = (a) => (f) => f(a);

const WITH_LOVE_FROM_JAVA = "with ♥ from java";
const SWEET_AND_LO_FI = "sweet & lo-fi";
const TK_NO = "tk?no";
const MUITO_NATURAL = "isto é muito natural";
const K_POP = "케이팝";

const renameAsMain = (str) =>
	str
		.split(/\r?\n/)
		.map((e) =>
			e.indexOf("module ") !== -1 && e.indexOf(" where") !== -1
				? "module Main where"
				: e
		)
		.join("\n");

const Hello = () => {
	const [[page, direction], setPage] = useState([0, 0]);
	const paginate = (newDirection) => {
		setPage([bindPage(page + newDirection), newDirection]);
	};
	const alert = useAlert();
	const handleError = (err) => () => {
		alert.error("Something went wrong. Our fault 🤦 Check the console 🙏", {
			timeout: 2000,
		});
		console.error(err);
	};
	// START ENGINE
  const [audioContext, setAudioContext] = useState(() => new AudioContext());
	const [currentPlaylistName, setCurrentPlaylistName] =
		useState(WITH_LOVE_FROM_JAVA);
	const [currentPlaylist, setCurrentPlaylist] = useState(simple);
	const [currentCompileError, setCurrentCompileError] = useState(null);
	const [code, setCode] = useState("");
	const [scrollIndex, setScrollIndex] = useState(0);
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
	console.log(isPlaying, "isPlaying");
	// END ENGINE
	// START FNs
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
		cleanErrorState: () => setCurrentCompileError(null),
		currentPlaylist,
		setCurrentPlaylist,
		audioContext,
		compileOnPlay: true,
		newWagPush,
		scrollIndex,
		setCode: eff(setCode),
		code,
		setStopScrolling: eff(setStopScrolling),
		setScrollIndex: eff(setScrollIndex),
		isScrolling,
		bufferCache,
		setIsScrolling: eff(setIsScrolling),
		ourFaultErrorCallback: handleError,
		yourFaultErrorCallback: (errs) => () => setCurrentCompileError(`${errs}`),
	});
	const wagsPlayer = playWags({
		scrollIndex,
		stopScrolling,
		setNewWagPush: eff(setNewWagPush),
		setCode: eff(setCode),
		setStopScrolling: eff(setStopScrolling),
		setScrollIndex: eff(setScrollIndex),
		setIsScrolling: eff(setIsScrolling),
		isPlaying: isPlaying,
		setAudioContext: eff(setAudioContext),
		setIsPlaying: eff(setIsPlaying),
		setStopWags: eff(setStopWags),
		bufferCache,
		currentPlaylist,
	});
	// loadingEffect
	const [loading, setLoading] = useState(true);
	useEffect(() => {
		setTimeout(() => {
			setLoading(false);
		}, 4000);
	}, []);
	const changePlaylist = setCurrentPlaylistName;
	return (
		<>
			<Motionable
				key={0}
				page={page}
				direction={direction}
				variant={page === 0 ? "presentMain" : "absentMain"}
			>
				<Listener>
					<R1C1>
						<WagsFM>wags.fm</WagsFM>
						<WagsUL>
							<WagsLI>
								<WagsLink onClick={() => changePlaylist(WITH_LOVE_FROM_JAVA)}>
									{WITH_LOVE_FROM_JAVA}
								</WagsLink>
							</WagsLI>
							<WagsLI>
								<WagsLink onClick={() => changePlaylist(SWEET_AND_LO_FI)}>
									{SWEET_AND_LO_FI}
								</WagsLink>
							</WagsLI>
							<WagsLI>
								<WagsLink onClick={() => changePlaylist(TK_NO)}>
									{TK_NO}
								</WagsLink>
							</WagsLI>
							<WagsLI>
								<WagsLink onClick={() => changePlaylist(MUITO_NATURAL)}>
									{MUITO_NATURAL}
								</WagsLink>
							</WagsLI>
							<WagsLI>
								<WagsLink onClick={() => changePlaylist(K_POP)}>
									{K_POP}
								</WagsLink>
							</WagsLI>
						</WagsUL>
					</R1C1>
					<R2C2>
						<FlexC>
							<Flex1></Flex1>
							<Flex0>
								<FlexR>
									<Flex1></Flex1>
									<Flex0>
										<IconCase onClick={isPlaying ? wagsStopper : wagsPlayer}>
											<FontAwesomeIcon
												someIcon
												size={"3x"}
												icon={isPlaying ? faStopCircle : faPlayCircle}
											></FontAwesomeIcon>
										</IconCase>
										<WagsNowPlaying>{currentPlaylistName}</WagsNowPlaying>
									</Flex0>
									<Flex1></Flex1>
								</FlexR>
							</Flex0>
							<Flex1></Flex1>
						</FlexC>
					</R2C2>
					<R3C3>
						<FlexC>
							<Flex1></Flex1>
							<Flex0>
								<FlexR>
									<Flex1></Flex1>
									<Flex0>
										<AnimatePresence>
											{isPlaying && (
												<motion.div
													variants={editMeVariants}
													initial='starting'
													animate='present'
													exit='absent'
													key={"editMe"}
												>
													<IconCase onClick={() => paginate(1)}>
														Edit me{" "}
														<FontAwesomeIcon
															icon={faChevronCircleDown}
														></FontAwesomeIcon>
													</IconCase>
												</motion.div>
											)}
										</AnimatePresence>
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
										Back{" "}
										<FontAwesomeIcon icon={faChevronCircleUp}></FontAwesomeIcon>
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
											<motion.div
												key={"code" + scrollIndex}
												style={{ position: "absolute", width: "100%" }}
												initial='starting'
												animate='present'
												exit='absent'
												variants={codeVariants}
												transition={{ duration: 1.2 }}
											>
												<deckgo-highlight-code
													onClick={scrollPauser}
													line-numbers={false}
													editable={true}
													language='haskell'
												>
													<code slot='code'>{code}</code>
												</deckgo-highlight-code>
											</motion.div>
										</AnimatePresence>
									</div>
								</Flex1>
								<Flex1>
									<AnimatePresence>
										{currentCompileError !== null && (
											<motion.div
												animate='present'
												initial='starting'
												exit='absent'
												variants={errorScreenVariants}
											>
												<deckgo-highlight-code
													onClick={scrollPauser}
													line-numbers={false}
													editable={true}
													language='bash'
												>
													<code slot='code'>{currentCompileError}</code>
												</deckgo-highlight-code>
											</motion.div>
										)}
									</AnimatePresence>
								</Flex1>
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
