function subscribeToConsole(app) {
  app.ports.log.subscribe((msg) => {
    console.error(msg);
  });
}

function subscribe(app) {
  subscribeToConsole(app);
}

export default {
  subscribe,
};
