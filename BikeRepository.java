package com.example.HelloAppEngine;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.HashMap;

import static java.nio.charset.StandardCharsets.UTF_8;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.cloud.ReadChannel;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobId;
import com.google.cloud.storage.BlobInfo;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.BucketInfo;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageOptions;
import com.google.common.net.HttpHeaders;

import javax.servlet.http.HttpServletResponse;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Repository;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

@Repository
public class BikeRepository {

    
    @Value("${bucketname}")
    private String bucketName;

    @Value("${openapikey}")
    private String openapikey;

    String now = System.getProperty("user.dir");

    @ResponseStatus(code = HttpStatus.NOT_FOUND, reason = "Directory Not Found")
    public class DirectoryNotFoundException extends RuntimeException {
    }

    // Create your service object
    // Storage storage = StorageOptions.getDefaultInstance().getService();

    // public Bucket getBucket() {
    // return storage.get(bucketName);
    // }

    public <GcsFileOptions> void getModelArtifact(String location, HttpServletResponse res)
            throws MalformedURLException, IOException {
        BufferedWriter bufWriter = null;

        // api가져오기
        String url = "http://openapi.seoul.go.kr:8088/" + openapikey + "/json/bikeList/1/10/";
        String genreJson = IOUtils.toString(new URL(url), Charset.defaultCharset());

        // System.out.println("as");
        // Bucket bucket = getBucket();
        // System.out.println(bucket);

    }

    public String getBikeList() {

        String csv = null;
        long time = System.currentTimeMillis();
        long time2 = System.currentTimeMillis();
        SimpleDateFormat dayTime = new SimpleDateFormat("yyyy-MM-dd-HH:mm");
        SimpleDateFormat dayTime2 = new SimpleDateFormat("yyyy-MM-dd");
        String str = dayTime.format(new Date(time));
        String str2 = dayTime2.format(new Date(time2));


        // 캘린더 지정
        Calendar cal = Calendar.getInstance();
        int hour = cal.get(Calendar.HOUR_OF_DAY);
        int minute = cal.get(Calendar.MINUTE);

        String url = "http://openapi.seoul.go.kr:8088/" + openapikey + "/json/bikeList/1/1000/";

        try {
            String genreJson = IOUtils.toString(new URL(url), Charset.defaultCharset());
            String JsonData = genreJson.substring(18, genreJson.length() - 1);
            JSONObject output = new JSONObject(JsonData);
            JSONArray docs = output.getJSONArray("row");

            // 현재의 인코딩
            String enc = new OutputStreamWriter(System.out).getEncoding();

            // csv 파일에 현재 시각 넣기
            for (int i = 0; i < docs.length(); i++) {
                JSONObject addData = (JSONObject) docs.get(i);
                addData.put("time", str);
            }

            csv = CDL.toString(docs);

            // Create a bucket
            // String bucketName = "bike_bucket1";
            // String blobName = "1_" + str + ".csv";

            // BlobId blobId = BlobId.of(bucketName, blobName);
            // BlobInfo blobInfo =
            // BlobInfo.newBuilder(blobId).setContentType("text/csv").build();
            // storage.create(blobInfo, csv.getBytes(UTF_8));

            File file = new File("/Users/kimyoungdong/workplace/tmp/1_" + str2 + ".csv");

            // FileWriter fw = new FileWriter(file, true);
            BufferedWriter writer = new BufferedWriter(new FileWriter(file, true));
            writer.write("\uFEFF");
            if (hour == 0 && minute == 0) {
                writer.write(csv);
            } else {
                writer.write(csv.substring(96));
            }
            writer.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return now;

    }

    public String getBikeList2() {

        String csv = null;
        long time = System.currentTimeMillis();
        long time2 = System.currentTimeMillis();
        SimpleDateFormat dayTime = new SimpleDateFormat("yyyy-MM-dd-HH:mm");
        SimpleDateFormat dayTime2 = new SimpleDateFormat("yyyy-MM-dd");
        String str = dayTime.format(new Date(time));
        String str2 = dayTime2.format(new Date(time2));

        // 캘린더 지정
        Calendar cal = Calendar.getInstance();
        int hour = cal.get(Calendar.HOUR_OF_DAY);
        int minute = cal.get(Calendar.MINUTE);

        String url = "http://openapi.seoul.go.kr:8088/" + openapikey + "/json/bikeList/1001/2000/";

        try {
            String genreJson = IOUtils.toString(new URL(url), Charset.defaultCharset());
            String JsonData = genreJson.substring(18, genreJson.length() - 1);
            JSONObject output = new JSONObject(JsonData);
            JSONArray docs = output.getJSONArray("row");

            // 현재의 인코딩
            String enc = new OutputStreamWriter(System.out).getEncoding();

            // csv 파일에 현재 시각 넣기
            for (int i = 0; i < docs.length(); i++) {
                JSONObject addData = (JSONObject) docs.get(i);
                addData.put("time", str);
            }

            csv = CDL.toString(docs);

            // Create a bucket
            // String bucketName = "bike_bucket2";
            // String blobName = "2_" + str + ".csv";

            // BlobId blobId = BlobId.of(bucketName, blobName);
            // BlobInfo blobInfo =
            // BlobInfo.newBuilder(blobId).setContentType("text/csv").build();
            // storage.create(blobInfo, csv.getBytes(UTF_8));

            File file = new File("/Users/kimyoungdong/workplace/tmp/2_" + str2 + ".csv");
            // FileWriter fw = new FileWriter(file, true);
            BufferedWriter writer = new BufferedWriter(new FileWriter(file, true));
            writer.write("\uFEFF");
            // System.out.println(str);
            // System.out.println(hour);
            // System.out.println(minute);

            if (hour == 0 && minute == 0) {
                writer.write(csv);
            } else {
                writer.write(csv.substring(96));
            }
            writer.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return now;

    }
}