export function json(j) {
  return function(options) {
    return Response.json(j, options)
  }
}


export function string(s) {
  return function(options) {
    return new Response(s, options)
  }
}


export function stream(s) {
  return function(options) {
    return new Response(s, options)
  }
}

export function body(resp) {
  return resp.body
}

export function status(resp) {
  return resp.status
}

export function statusText(resp) {
  return resp.statusText
}

export function headers(resp) {
  return Object.fromEntries(resp.headers.entries());
}
