package com.example.HelloAppEngine;

import java.sql.SQLException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@CrossOrigin
public class BikeController {
    @Autowired
    private BikeRepository bikeRepo;

    @RequestMapping(value = "/shared", method = RequestMethod.GET)
    public String getModelList() throws ClassNotFoundException, SQLException {
        return bikeRepo.getBikeList();
    }
    @RequestMapping(value = "/shared2", method = RequestMethod.GET)
    public String getModelList2() throws ClassNotFoundException, SQLException {
        return bikeRepo.getBikeList2();
    }

    @RequestMapping(value = "/model-artifact/**", method = RequestMethod.GET)
    public void getModelArtifact(HttpServletRequest req, HttpServletResponse res)
            throws ClassNotFoundException, SQLException {
        String dataLocation = new AntPathMatcher()
            .extractPathWithinPattern("/model-artifact/**", req.getRequestURI());
        // System.out.println(req);
        // System.out.println(dataLocation);
        // System.out.println("22");
        // bikeRepo.getModelArtifact(dataLocation, res);
    }    
}