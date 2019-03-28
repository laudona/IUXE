<template>
  <v-app dark>
    <v-toolbar app color="primary">
      <v-toolbar-title class="headline text-uppercase">
        <span>Purring Butter</span>
        <span class="font-weight-light"> Event Mocking</span>
      </v-toolbar-title>
      <v-spacer></v-spacer>
      <v-btn icon href="/public/spotify/login.html" target="_blank">
          <v-icon>fab fa-spotify</v-icon>
      </v-btn>
    </v-toolbar>
    <v-content>
    <v-container fluid>
      <router-view></router-view>
    </v-container>
  </v-content>
  </v-app>
</template>

<script>

export default {
  name: 'App',
  data () {
    return {
      //
    }
  }
}
</script>

<style>
.container {
    max-width: 1920;
}
</style>

