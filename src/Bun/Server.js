export function serve_(f) {
  return function() {
    return Bun.serve({
      fetch(req) {
        return f(req)()
      }
    })
  }
}
