<template>
  <NuxtRouteAnnouncer />
  <NuxtLoadingIndicator />

  <UApp :toaster="{ position: 'bottom-center' }">
    <NuxtPage />
  </UApp>
</template>
