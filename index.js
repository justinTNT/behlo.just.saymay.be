

const toSong = volume => track => {
  return {
    name          : track.title,
    artist        : "Behlo",
    album         : volume.title,
    url           : track.url,
    cover_art_url : track.url.replace(/[^/]*.mp3/, `${volume.tag}.jpg`)
  };
};

export default {
  load: async function (elmLoaded) {
      const app = await elmLoaded;
      app.ports.amplitude.subscribe(function(volume) {
          const initObj =
              { songs: volume.trax.map(toSong(volume)),
                callbacks: {
                    play: () => { document.getElementById('amplitude-left').classList.add('playing'); },
                    pause: () => { document.getElementById('amplitude-left').classList.remove('playing'); }
                },
                volume: 100,
              };
          let anode = Amplitude.getAnalyser();
          if (anode) {
            Amplitude.pause();
            anode.context.close().then(() => {
                  Amplitude.init(initObj);
            });
          } else {
                  Amplitude.init(initObj);
          }
      });
  },
  flags: function () {
    return null;
  },
};
