package de.mi.server;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Application;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import java.util.HashSet;
import java.util.Set;

@Path("/")
class LiteratureRest {

    public static Class<? extends Application> getApplicationClass() {
        return InnerApplication.class;
    }

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public Response getName() {
        return Response.ok("Informatik Fachliteratur").build();
    }

    // TODO: add request handler

    private static class InnerApplication extends Application {
        private static final Set<Class<?>> CLASSES = new HashSet<>();

        public InnerApplication() {
            CLASSES.add(LiteratureRest.class);
        }

        @Override
        public Set<Class<?>> getClasses() {
            return CLASSES;
        }
    }
}
