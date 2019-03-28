<template>
  <v-container fluid>
    <v-layout>
      <v-flex class="pr-3 xs3">
        <v-card color="info">
          <v-toolbar card prominent color="secondary">
            <v-toolbar-title class="body-4 accent--text">Simulate Event</v-toolbar-title>
            <v-spacer></v-spacer>
            <v-menu offset-y>
              <template v-slot:activator="{ on }">
                <v-btn icon v-on="on">
                  <v-icon>more_vert</v-icon>
                </v-btn>
              </template>
              <v-list>
                <v-list-tile v-for="(item, index) in menuItems" :key="index" @click=clickMenu(item)>
                  <v-list-tile-title>{{ item.title }}</v-list-tile-title>
                </v-list-tile>
              </v-list>
            </v-menu>
          </v-toolbar>
          <v-divider></v-divider>
          <v-text-field
            label="Event Name"
            placeholder="Enter event in '<role>.event.<name>' format here"
            v-model="eventName"
            single-line
            full-width
            hide-details
          ></v-text-field>
          <v-divider></v-divider>
          <v-textarea
            v-model="eventData"
            label="Event Data"
            placeholder="Enter event data in ttl format here."
            counter
            maxlength="5000"
            full-width
            auto-grow
            single-line
          ></v-textarea>
          <v-card-actions>
            <v-spacer></v-spacer>
            <v-btn color="secondary" @click="sendEvent">Send</v-btn>
          </v-card-actions>
        </v-card>
      </v-flex>
      <v-flex class="pl-3" xs9>
        <v-card color="info">
          <v-toolbar card prominent color="secondary">
            <v-toolbar-title class="body-4 accent--text">Received Actions and Events</v-toolbar-title>
            <v-spacer></v-spacer>
          </v-toolbar>
          <v-divider></v-divider>
          <v-card-text>
            <v-card
              :color="item.action ? 'secondary' : 'primary'"
              class="mb-2"
              :key="index"
              v-for="(item, index) in items"
            >
              <v-card-title primary-title class="py-1">
                <h3 class="headline mb-0">{{item.action || item.event}}</h3>
              </v-card-title>
              <v-divider></v-divider>
              <v-card-text>
                <pre>{{item.data}}</pre>
              </v-card-text>
            </v-card>
          </v-card-text>
          <v-card-actions>
            <v-spacer></v-spacer>
            <v-btn color="secondary" @click="clearItems">Clear</v-btn>
          </v-card-actions>
        </v-card>
      </v-flex>
    </v-layout>
  </v-container>
</template>

<script>
/* eslint-disable no-console */
import connectServer from "../client";
import examples from "../examples"

export default {
  data: () => ({
    eventName: "pepper.event.sees",
    eventData: "",

    items: [
      /*

      Example action and events:

      { type: 'action', action: 'My Action', data: '<pepper> <say> "hello, there!" . ', dataType: 'text/turtle' },
      { type: 'event', event: 'My Event', data: '<pepper> <sees> <person5> . ', dataType: 'text/turtle' }
      */
    ],
    menuItems: examples
  }),
  created() {
    this.server = connectServer("lur", "SfMnwiiTjZ");
    this.server.on("**", data => this.addItem(data));
  },
  methods: {
    addItem(item) {
      console.log(`Adding item '${item.action || item.event}'...`);
      this.items.unshift(item);
    },

    clearItems() {
      console.log(`Clearing ${this.items.length} items...`);
      this.items = [];
    },

    clickMenu(item) {
      console.log(`Menu item ${item.title} clicked...`);
      this.eventName = item.action || item.event;
      this.eventData = item.data;
    },

    sendEvent() {
      if (this.eventName.indexOf("action") > -1) {
        this.server.sendAction(this.eventName, this.eventData);  
      } else {
        this.server.sendEvent(this.eventName, this.eventData);  
      }
    }

  }
};
</script>

<style>
</style>
