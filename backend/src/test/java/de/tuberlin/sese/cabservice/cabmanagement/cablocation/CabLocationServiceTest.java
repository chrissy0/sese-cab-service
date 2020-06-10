package de.tuberlin.sese.cabservice.cabmanagement.cablocation;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class CabLocationServiceTest {

    @Autowired
    private CabLocationService service;

    @MockBean
    private CabLocationRepo repo;

    @Captor
    private ArgumentCaptor<CabLocationEntity> entityCaptor;

    @Test
    public void shouldGetAllCabLocations() {
        CabLocationEntity entity1 = CabLocationEntity.builder()
                .cabId(1L)
                .section(12)
                .build();
        CabLocationEntity entity2 = CabLocationEntity.builder()
                .cabId(5L)
                .section(2)
                .build();

        when(repo.findAll()).thenReturn(asList(entity1, entity2));

        List<CabLocationEntity> locations = service.getAllCabLocations();

        verify(repo).findAll();
        verifyNoMoreInteractions(repo);

        assertThat(locations)
                .usingRecursiveFieldByFieldElementComparator()
                .containsExactly(entity1, entity2);
    }

    @Test
    public void shouldSaveCabLocation() {
        CabLocationEntity entity = CabLocationEntity.builder()
                .cabId(4L)
                .section(15)
                .build();

        service.saveCabLocation(entity);

        verify(repo).findById(4L);
        verify(repo).save(entityCaptor.capture());
        verifyNoMoreInteractions(repo);

        assertThat(entityCaptor.getValue().getCabId()).isEqualTo(4);
        assertThat(entityCaptor.getValue().getSection()).isEqualTo(15);
    }
}
