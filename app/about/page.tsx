export default function AboutPage() {
  return (
    <section>
      <h1 className="font-bold text-3xl font-serif">About Me</h1>
      <div className="prose prose-neutral dark:prose-invert text-neutral-800 dark:text-neutral-200">
        <p className="my-5 text-neutral-800 dark:text-neutral-200">
          I'm currently a developer at the <strong> Ethereum Foundation</strong>
          , where I am working on the UniRep protocol and several other Web3
          projects.{" "}
        </p>
        <hr></hr>
        <p className="my-5 text-neutral-800 dark:text-neutral-200">
          Over the past few years, physics (
          <strong>particularly astrophysics</strong>) has become something that
          I have spent a lot of time thinking about. Computational physics and
          numerical methods as a whole is pretty cool.
        </p>
        <p className="my-5 text-neutral-800 dark:text-neutral-200">
          Cryptography and zero-knowledge proofs (ZKP) has evolved from a
          side-project to one of my focuses over the past few months. The
          combination of the mathematics of elliptic-curve cryptography along
          with others in addition to developing the Web3.0 environment continues
          to fascinate me.
        </p>
        <p className="my-5 text-neutral-800 dark:text-neutral-200">
          Generally speaking, I have always been attracted to puzzles, notably
          chess over the past few years. I suppose this has organically inspired
          my interest in computer science, math, and physics. Check out some of
          my projects on my Github.
        </p>
        <div className="flex flex-col gap-2 md:flex-row md:gap-2">
          <a
            rel="noopener noreferrer"
            target="_blank"
            href="https://drive.google.com/file/d/1GNfm-TDSluk8LbnDkUwyEIdGRa-fkwtb/view?usp=sharing"
            className="flex w-full border border-neutral-200 dark:border-neutral-800 rounded-lg p-4 no-underline items-center text-neutral-800 dark:text-neutral-200 hover:dark:bg-neutral-900 hover:bg-neutral-100 transition-all justify-between"
          >
            <div className="flex items-center">
              <div className="ml-3">Resume</div>
            </div>
            <svg
              className="mr-2"
              width="12"
              height="12"
              viewBox="0 0 12 12"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M2.07102 11.3494L0.963068 10.2415L9.2017 1.98864H2.83807L2.85227 0.454545H11.8438V9.46023H10.2955L10.3097 3.09659L2.07102 11.3494Z"
                fill="currentColor"
              ></path>
            </svg>
          </a>
          <a
            rel="noopener noreferrer"
            target="_blank"
            href="https://github.com/0xWOLAND"
            className="flex w-full border border-neutral-200 dark:border-neutral-800 rounded-lg p-4 no-underline items-center text-neutral-800 dark:text-neutral-200 hover:dark:bg-neutral-900 hover:bg-neutral-100 transition-all justify-between"
          >
            <div className="flex items-center">
              <div className="ml-3">Github</div>
            </div>
            <svg
              className="mr-2"
              width="12"
              height="12"
              viewBox="0 0 12 12"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M2.07102 11.3494L0.963068 10.2415L9.2017 1.98864H2.83807L2.85227 0.454545H11.8438V9.46023H10.2955L10.3097 3.09659L2.07102 11.3494Z"
                fill="currentColor"
              ></path>
            </svg>
          </a>
          <a
            rel="noopener noreferrer"
            target="_blank"
            href="https://www.linkedin.com/in/bhargav-a-672777180/?trk=public-profile-join-page"
            className="flex w-full border border-neutral-200 dark:border-neutral-800 rounded-lg p-4 no-underline items-center text-neutral-800 dark:text-neutral-200 hover:dark:bg-neutral-900 hover:bg-neutral-100 transition-all justify-between"
          >
            <div className="flex items-center">
              <div className="ml-3">LinkedIn</div>
            </div>
            <svg
              className="mr-2"
              width="12"
              height="12"
              viewBox="0 0 12 12"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M2.07102 11.3494L0.963068 10.2415L9.2017 1.98864H2.83807L2.85227 0.454545H11.8438V9.46023H10.2955L10.3097 3.09659L2.07102 11.3494Z"
                fill="currentColor"
              ></path>
            </svg>
          </a>
        </div>
      </div>
      <div className="prose prose-neutral dark:prose-invert text-neutral-800 dark:text-neutral-200">
        <h1 className="font-bold text-3xl font-serif">Publications</h1>
        <p className="my-5 text-neutral-800 dark:text-neutral-200">
          Bhargav Annem, Sergey Khoperskov, Impact of orbiting satellites on
          star formation rate evolution and metallicity variations in Milky
          Way-like discs,
          <i> Monthly Notices of the Royal Astronomical Society</i>, 2023;,
          stad3244,{" "}
          <a href="https://doi.org/10.1093/mnras/stad3244">
            https://doi.org/10.1093/mnras/stad3244
          </a>
        </p>
        <p className="my-5 text-neutral-800 dark:text-neutral-200">
          Hopefully more to come...
        </p>
      </div>
    </section>
  );
}
