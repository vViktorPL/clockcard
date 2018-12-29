const { protocol: electronProtocol } = require('electron');
const { PassThrough } = require('stream');

const parseSimplePath = path => {
  // Check for named placeholder crowding
  if (/\:[a-zA-Z0-9]+\:[a-zA-Z0-9]+/g.test(path)) {
    throw new Error('Invalid usage of named placeholders');
  }

  // Check for mixed placeholder crowdings
  var mixedPlaceHolders =
    /(\*\:[a-zA-Z0-9]+)|(\:[a-zA-Z0-9]+\:[a-zA-Z0-9]+)|(\:[a-zA-Z0-9]+\*)/g;
  if (mixedPlaceHolders.test(path.replace(/\\\*/g,''))) {
    throw new Error('Invalid usage of named placeholders');
  }

  // Try parsing the string and converting special characters into regex
  try {
    // Parsing anonymous placeholders with simple backslash-escapes
    path = path.replace(/(.|^)[*]+/g, function(m,escape) {
      return escape==='\\' ? '\\*' : (escape+'(?:.*?)');
    });

    // Parsing named placeholders with backslash-escapes
    var tags = [];
    path = path.replace(/(.|^)\:([a-zA-Z0-9]+)/g, function (m, escape, tag) {
      if (escape === '\\') { return ':' + tag; }
      tags.push(tag);
      return escape + '(.+?)';
    });

    return { regexp: RegExp(path + '$'), tags: tags };
  }

    // Failed to parse final path as a RegExp
  catch (ex) {
    throw new Error('Invalid path specified');
  }
};

function createStream (text) {
  const rv = new PassThrough(); // PassThrough is also a Readable stream
  rv.push(text);
  rv.push(null);
  return rv;
}

const urlToPath = url => url.split(':').slice(1).join(':');

const createResponse = callback => ({
  send(data) {
    callback({
      mimeType: typeof data !== 'object' ? 'application/text' : 'application/json',
      data: Buffer.from(typeof data !== 'object' ? data : JSON.stringify(data))
    });
  }
});

const createRequest = ({ regexp, tags }, request) => ({
  params: regexp.exec(urlToPath(request.url)).slice(1).reduce(
    (params, value, index) => {
      params[tags[index]] = value;
      return params;
    },
    {}
  ),
  body: 'uploadData' in request ? JSON.parse(request.uploadData[0].bytes.toString('utf8')) : void 0,
});


module.exports = function elexpress() {
  const routes = [];

  const handleRequest = protocol =>
    (request, callback) => {
    console.log(request);
      const url = request.url.slice(protocol.length + 1);
      const routeMatch = routes.find(([method, pattern]) => method === request.method && pattern.regexp.test(url));

      if (!routeMatch) {
        callback({ error: -300 });
        return;
      }

      const [, pattern, routeCallback] = routeMatch;

      routeCallback(createRequest(pattern, request), createResponse(callback));
    };

  return ['GET', 'POST', 'PATCH', 'PUT', 'DELETE'].reduce(
    (app, method) => {
      app[method.toLowerCase()] = (pattern, callback) => {
        routes.push([method, parseSimplePath(pattern), callback]);
      };

      return app;
    },
    {
      listen(protocol = 'backend') {
        electronProtocol.registerBufferProtocol(protocol, handleRequest(protocol))
      },
    }
  );
};