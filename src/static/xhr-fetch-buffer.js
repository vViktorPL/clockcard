// Patch XHR due to the problems with sending POST payloads (upload data)
// to non HTTP protocols in Electron/Chromium environment.

const readBlobContentsAndMimeType = blob => new Promise((resolve) => {
  const reader = new FileReader();

  reader.onloadend = (e) => {
    resolve([e.srcElement.result, blob.type])
  };

  reader.readAsText(blob);
});

exports.default = window.XMLHttpRequest = class extends XMLHttpRequest {
  open(verb, url, async = true) {
    if (url.indexOf('backend:') === 0) {
      if (!async) {
        throw new Error('sync requests not supported');
      }

      this._headers = {};
      this._requestVerb = verb;
      this._requestUrl = url;
      return;
    }

    super.open(verb, url, async);
  }

  setRequestHeader(key, value) {
    if (this._requestUrl) {
      this._headers[key] = value;
      return;
    }

    super.setRequestHeader(key, value);
  }

  getAllResponseHeaders() {
    if (!this._requestUrl) {
      return super.getAllResponseHeaders();
    }

    return this._responseHeaders;
  }

  send(body) {
    if (!this._requestUrl) {
      super.send(body);
      return;
    }

    fetch(this._requestUrl, {
      method: this._requestVerb,
      headers: this._headers,
      body
    })
      .then(response => {
        Object.defineProperty(this, 'status', { value: response.status });
        Object.defineProperty(this, 'statusText', { value: response.statusText });
        Object.defineProperty(this, 'responseURL', { value: response.responseURL });

        return response.blob();
      })
      .then(readBlobContentsAndMimeType)
      .then(([contents, mimeType]) => {
        Object.defineProperty(this, 'response', { value: contents });
        this._responseHeaders = `Content-Type: ${mimeType}\r\n`;

        this.dispatchEvent(new Event('load'));
      })
      .catch(errorDetails => {
        this.dispatchEvent(new Event('error', errorDetails));
      });
  }
};