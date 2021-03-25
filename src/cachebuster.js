async function ensureReload() {
    const cacheName = 'workbox-precache-v2-https://alpha.funklang.com/'

    const registrations = await navigator.serviceWorker.getRegistrations()

    for (let registration of registrations) {
        registration.unregister()
    }

    const hasCache = await caches.has(cacheName)
    if (hasCache) {
        await caches.delete(cacheName)
        window.location.reload()
    }
}

ensureReload()
