export function jsonPromise(req) {
  return req.json()
}

export function method(req) {
  return req.method
}

export function url(req) {
  return req.url
}

export function headers(req) {
  return Object.fromEntries(req.headers.entries());
}
